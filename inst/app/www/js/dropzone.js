// Drag-and-drop for the Step 1 upload cards.
// The dashed zone is only a shell: Shiny's own fileInput lives inside it and
// owns the upload. A click already works (the file-picker label is stretched
// over the zone by 04-components.css); this script adds the "arraste" half of
// the promise by handing the dropped file to that same input and letting
// Shiny's own change handler take it from there.
(function () {
  "use strict";

  function fileInputOf(zone) {
    return zone.querySelector('input[type="file"]');
  }

  document.addEventListener("dragover", function (e) {
    var zone = e.target.closest ? e.target.closest("[data-dropzone]") : null;
    if (!zone) return;
    e.preventDefault();
    zone.classList.add("is-dragover");
  });

  document.addEventListener("dragleave", function (e) {
    var zone = e.target.closest ? e.target.closest("[data-dropzone]") : null;
    if (zone) zone.classList.remove("is-dragover");
  });

  document.addEventListener("drop", function (e) {
    var zone = e.target.closest ? e.target.closest("[data-dropzone]") : null;
    if (!zone) return;
    e.preventDefault();
    zone.classList.remove("is-dragover");

    var input = fileInputOf(zone);
    if (!input || !e.dataTransfer || e.dataTransfer.files.length === 0) return;

    // Assigning a FileList straight from the drop event is what makes Shiny's
    // upload run: it binds to the input's change event, not to the drop.
    input.files = e.dataTransfer.files;
    input.dispatchEvent(new Event("change", { bubbles: true }));
  });
})();
