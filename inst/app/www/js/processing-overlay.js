// Dims the processing step while Shiny's withProgress notification is on screen,
// so only the progress toast stays in focus. This is client-side by necessity:
// a normal sendCustomMessage would not flush mid-observer, but Shiny streams the
// progress notification live during the synchronous run, so we watch the
// notification panel for it and toggle a class on <body>.
(function () {
  function isProcessing() {
    // Shiny's notification-style withProgress renders .shiny-progress-notification
    // inside #shiny-notification-panel (plain toasts do not carry this class).
    return document.querySelector(".shiny-progress-notification") !== null;
  }
  function sync() {
    document.body.classList.toggle("zh-processing", isProcessing());
  }

  // Coalesce bursts (DataTables, leaflet redraws) into one check per frame.
  var scheduled = false;
  function schedule() {
    if (scheduled) return;
    scheduled = true;
    requestAnimationFrame(function () {
      scheduled = false;
      sync();
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    sync();
    // childList/subtree only — we never watch attributes, so toggling the body
    // class cannot re-trigger the observer.
    new MutationObserver(schedule).observe(document.body, {
      childList: true,
      subtree: true
    });
  });
})();
