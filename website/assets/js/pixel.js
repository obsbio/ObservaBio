/* Motion for the landing page. Two pieces, both optional: the page is fully
 * readable with JavaScript disabled, and anyone who asked the system for less
 * motion gets none of it.
 *
 *   reveal()  softens how each block arrives on scroll
 *   flow()    draws the hero pipeline — four raw streams entering from off the
 *             screen, converging into the ObservaBio core and coming back out
 *             validated before leaving by the other edge
 */
(function () {
  'use strict';

  var slice = function (nodes) { return Array.prototype.slice.call(nodes); };

  if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
    return; // the CSS fallbacks are all static, so there is nothing to undo
  }

  reveal();
  flow();

  /* ---------------------------------------------------------------- reveal */
  function reveal() {
    // The hero is deliberately absent: what sits on the fold has to be there on
    // the first paint, and a transform on `.pipe` would slide the cards out of
    // register with the canvas, which measures them from the hero's own box.
    var nodes = slice(document.querySelectorAll([
      '.stat',
      '.section-head',
      '.geo-text',
      '.geo-fig',
      '.output'
    ].join(',')));

    if (!('IntersectionObserver' in window)) {
      return; // .reveal is never applied, so everything stays visible
    }

    nodes.forEach(function (node, i) {
      node.classList.add('reveal');
      // Stagger only within a group, so a long list never trails off-screen.
      node.style.transitionDelay = (i % 4) * 70 + 'ms';
    });

    var observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (!entry.isIntersecting) return;
        entry.target.classList.add('is-in');
        observer.unobserve(entry.target);
      });
    }, { rootMargin: '0px 0px -8% 0px', threshold: 0.08 });

    nodes.forEach(function (node) { observer.observe(node); });
  }

  /* ------------------------------------------------------------------ flow
   * A canvas laid over the whole hero. Every coordinate is measured from the
   * cells themselves, so the drawing follows the layout at any width instead
   * of hard-coding a shape, and every colour is a design token.
   *
   * What it shows is the app's own gesture: a raw pixel arrives from off the
   * screen, passes through the field it belongs to, the four streams braid
   * into a single point at the core, and each pixel comes out the other side
   * in the colour of its verdict before leaving by the far edge. Nothing is
   * emitted on the right that did not arrive on the left. */
  function flow() {
    var pipe = document.querySelector('.pipe');
    if (!pipe || !window.requestAnimationFrame) return;

    var grid = pipe.querySelector('.pipe-grid');
    var core = pipe.querySelector('.pipe-core');
    var rows = slice(pipe.querySelectorAll('.pipe-row'));
    if (!grid || !core || rows.length === 0) return;

    /* The canvas hangs off the hero, not off the grid. The cards stop at the
     * text column, but the streams have to reach the edges of the screen —
     * that run in from off-page is what makes the hero read as a flow rather
     * than as a diagram in a box. */
    var host = document.querySelector('.hero-flow') || grid;

    var canvas = document.createElement('canvas');
    var ctx = canvas.getContext ? canvas.getContext('2d') : null;
    if (!ctx) return;
    canvas.className = 'pipe-canvas';
    canvas.setAttribute('aria-hidden', 'true');
    host.appendChild(canvas);

    var vars = getComputedStyle(document.documentElement);
    var token = function (name, fallback) {
      return vars.getPropertyValue(name).trim() || fallback;
    };
    var RAIL = token('--border-default', 'rgba(34,40,38,.16)');
    var RAW  = token('--secondary', '#C86446');
    var DONE = token('--success', '#277148');

    // One speed for every leg, in px/ms. Timing a leg instead would make the
    // short hop off the edge of a narrow screen crawl while the long run
    // across a wide one sprints — the streams would stop reading as one belt.
    var SPEED = 0.28;
    var HOLD = 300;    // ms a pixel spends inside the core before coming out
    var GAP = 270;     // ms between emissions on the same row: dense enough to
                       // read as a stream rather than as a countdown
    var CYCLE = 9000;  // must match the .frame cross-fade in theme.scss
    var SIZE = 6;      // a pixel, in CSS px
    var FADE = 64;     // px of alpha ramp at each edge, so nothing pops in

    // The stacked layout has the input sitting on top of the output, where a
    // left-to-right curve means nothing. There the CSS wire does the job.
    var wide = window.matchMedia('(min-width: 821px)');

    // A lane's four legs. Index 1 ends at the core and index 2 leaves it, so
    // those are the two the hold sits between and the two that braid.
    var FEED = 0, IN = 1, OUT = 2, AWAY = 3;

    var geom = null, gate = null, size = { w: 0, h: 0 };
    var parts = [], pulses = [], held = [];
    var nextEmit = rows.map(function (_, i) { return i * 200; });
    var visible = true, raf = null, last = 0, clock = 0;

    function curve(p0, p3) {
      var dx = (p3.x - p0.x) * 0.55;
      return [p0, { x: p0.x + dx, y: p0.y }, { x: p3.x - dx, y: p3.y }, p3];
    }

    // A leg carries its own duration, derived from how far it actually runs.
    function leg(p0, p3) {
      var dx = p3.x - p0.x, dy = p3.y - p0.y;
      return {
        c: curve(p0, p3),
        ms: Math.max(1, Math.sqrt(dx * dx + dy * dy) / SPEED)
      };
    }

    function at(c, t) {
      var m = 1 - t;
      var a = m * m * m, b = 3 * m * m * t, d = 3 * m * t * t, e = t * t * t;
      return {
        x: a * c[0].x + b * c[1].x + d * c[2].x + e * c[3].x,
        y: a * c[0].y + b * c[1].y + d * c[2].y + e * c[3].y
      };
    }

    function measure() {
      var box = host.getBoundingClientRect();
      var dpr = Math.min(window.devicePixelRatio || 1, 2);
      size = { w: box.width, h: box.height };
      canvas.width = Math.max(1, Math.round(box.width * dpr));
      canvas.height = Math.max(1, Math.round(box.height * dpr));
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

      var cr = core.getBoundingClientRect();
      gate = { x: cr.left - box.left, y: cr.top - box.top + cr.height / 2 };
      var exit = { x: cr.right - box.left, y: gate.y };

      geom = rows.map(function (row) {
        var a = row.querySelector('.cell-in').getBoundingClientRect();
        var b = row.querySelector('.cell-out').getBoundingClientRect();
        var ya = a.top - box.top + a.height / 2;
        var yb = b.top - box.top + b.height / 2;
        // The two middle legs stop at the edges of the cards, never under
        // them: the card is opaque and above the canvas, so a pixel crossing
        // one is swallowed and comes out the far side. That is the point.
        return [
          leg({ x: 0, y: ya }, { x: a.left - box.left, y: ya }),
          leg({ x: a.right - box.left, y: ya }, gate),
          leg(exit, { x: b.left - box.left, y: yb }),
          leg({ x: b.right - box.left, y: yb }, { x: size.w, y: yb })
        ];
      });
    }

    /* Where the .frame cross-fade is in its cycle, `ahead` ms from now. Asking
     * the animation itself beats counting from a clock of our own: the two only
     * agree if they share an origin, and ours starts when the section scrolls
     * into view, not when the page paints. */
    var clockEl = pipe.querySelector('.cell-out .frame-a');

    function phaseAt(ahead) {
      var anim = clockEl && clockEl.getAnimations ? clockEl.getAnimations()[0] : null;
      var t = (anim && anim.currentTime != null)
        ? anim.currentTime
        : (window.performance && performance.now ? performance.now() : 0);
      return (t + ahead) % CYCLE;
    }

    /* Which row raises an alert on which record is the page's business, not
     * this file's: each row declares it as data-alert="a" or "b". The colour
     * comes from that row's own chip, so a pixel always lands in the colour of
     * the badge it is delivering — amber for the distribution flag, red for the
     * invasive cross-check, because those are two axes and not two volumes. */
    var alerts = rows.map(function (row) {
      var when = row.getAttribute('data-alert');
      if (!when) return null;
      var chip = row.querySelector('.frame-' + when + ' .chip');
      return {
        when: when,
        colour: chip ? getComputedStyle(chip).color : token('--warning-ink', '#8A5A00')
      };
    });

    /* The pixel is coloured by the frame that will be showing when it lands,
     * not by the one showing when it leaves. */
    function verdict(lane) {
      var alert = alerts[lane];
      if (!alert) return DONE;
      var phase = phaseAt(HOLD + geom[lane][OUT].ms);
      var showing = (phase > CYCLE * 0.47 && phase < CYCLE * 0.95) ? 'b' : 'a';
      return alert.when === showing ? alert.colour : DONE;
    }

    function emit(lane, from, colour) {
      parts.push({
        lane: lane,
        leg: from,
        t: 0,
        colour: colour,
        // A stream where every pixel keeps rank reads as a conveyor belt. The
        // spread in speed and offset is what makes the four braid at the gate.
        speed: 0.85 + Math.random() * 0.3,
        wobble: (Math.random() * 2 - 1) * 3.5
      });
    }

    function step(now) {
      raf = null;
      if (!visible || !wide.matches) return;

      // A backgrounded tab must not fast-forward, and a frame timestamp that
      // predates the one we started from must not rewind.
      var dt = Math.max(0, Math.min(now - last, 50));
      last = now;
      clock += dt;

      geom.forEach(function (_, lane) {
        if (clock < nextEmit[lane]) return;
        emit(lane, FEED, RAW);
        nextEmit[lane] = clock + GAP + Math.random() * 160;
      });

      parts = parts.filter(function (p) {
        p.t += dt / (geom[p.lane][p.leg].ms * p.speed);
        if (p.t < 1) return true;
        if (p.leg === IN) {
          // Absorbed by the core; what comes out later is the same pixel, judged.
          if (pulses.length < 3) pulses.push({ t: 0 });
          held.push({ lane: p.lane, due: clock + HOLD });
          return false;
        }
        if (p.leg === AWAY) return false;
        // Crossing a card is not a new pixel: the same one carries on.
        p.leg += 1;
        p.t = 0;
        return true;
      });

      // The hold is counted on the loop's own clock, not on a timer: a pixel
      // parked inside the core while the hero is off-screen has to still be
      // there when the reader comes back.
      held = held.filter(function (h) {
        if (clock < h.due) return true;
        emit(h.lane, OUT, verdict(h.lane));
        return false;
      });

      pulses = pulses.filter(function (u) {
        u.t += dt / 420;
        return u.t < 1;
      });

      draw();
      schedule();
    }

    function draw() {
      ctx.clearRect(0, 0, size.w, size.h);

      // The rails run to the very edges of the screen and are cut off there,
      // which is what says the flow does not begin or end on this page.
      ctx.strokeStyle = RAIL;
      ctx.lineWidth = 2;
      ctx.setLineDash([3, 5]);
      geom.forEach(function (legs) {
        legs.forEach(function (l) { rail(l.c); });
      });
      ctx.setLineDash([]);

      pulses.forEach(function (u) {
        var s = SIZE + 14 * u.t;
        ctx.globalAlpha = 0.55 * (1 - u.t);
        ctx.strokeStyle = RAW;
        ctx.strokeRect(
          Math.round(gate.x - s / 2), Math.round(gate.y - s / 2), s, s
        );
      });
      ctx.globalAlpha = 1;

      parts.forEach(function (p) {
        var braid = (p.leg === IN || p.leg === OUT);
        var pt = at(geom[p.lane][p.leg].c, p.t);
        // The wobble goes to zero at both ends: the streams braid in the middle
        // and still land exactly on the cell they belong to. The legs that run
        // off the page stay straight — they are travel, not decision.
        var y = braid ? pt.y + p.wobble * Math.sin(p.t * Math.PI) : pt.y;
        ctx.globalAlpha = Math.max(0, Math.min(
          1, pt.x / FADE, (size.w - pt.x) / FADE
        ));
        ctx.fillStyle = p.colour;
        ctx.fillRect(
          Math.round(pt.x - SIZE / 2), Math.round(y - SIZE / 2), SIZE, SIZE
        );
      });
      ctx.globalAlpha = 1;
    }

    function rail(c) {
      ctx.beginPath();
      ctx.moveTo(c[0].x, c[0].y);
      ctx.bezierCurveTo(c[1].x, c[1].y, c[2].x, c[2].y, c[3].x, c[3].y);
      ctx.stroke();
    }

    function schedule() {
      if (raf === null) raf = window.requestAnimationFrame(step);
    }

    function start() {
      var on = wide.matches;
      pipe.classList.toggle('has-flow', on);
      canvas.classList.toggle('is-off', !on);
      if (!on) return;
      measure();
      last = (window.performance && performance.now) ? performance.now() : 0;
      schedule();
    }

    // Off-screen the loop stops entirely rather than painting to nobody.
    if ('IntersectionObserver' in window) {
      new IntersectionObserver(function (entries) {
        visible = entries[0].isIntersecting;
        if (visible) {
          last = (window.performance && performance.now) ? performance.now() : 0;
          schedule();
        }
      }, { threshold: 0.05 }).observe(pipe);
    }

    // Fonts landing late change the cell heights, so the rails are re-measured
    // from the layout rather than trusted from first paint. Both boxes are
    // watched: the hero decides where the edges of the screen are, the grid
    // decides where the cards sit inside it.
    function remeasure() {
      if (!wide.matches) return;
      measure();
      draw();
    }

    if ('ResizeObserver' in window) {
      var ro = new ResizeObserver(remeasure);
      ro.observe(grid);
      if (host !== grid) ro.observe(host);
    } else {
      window.addEventListener('resize', remeasure);
    }

    if (wide.addEventListener) {
      wide.addEventListener('change', start);
    } else if (wide.addListener) {
      wide.addListener(start);
    }

    start();
  }
})();
