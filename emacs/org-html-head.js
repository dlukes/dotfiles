function darkModeToggle() {
  var dark = "dark";
  var dcl = document.documentElement.classList;
  var lights = document.getElementById("lights");
  // TODO: I originally found the SVG icons here:
  // https://fhur.me/posts/always-use-closed-open-intervals
  // Not entirely sure what their licensing situation is, replace them with
  // unequivocally free ones at some point.
  if (dcl.contains(dark)) {
    lights.innerHTML = '<svg fill="none" viewBox="0 0 24 24" width="24" height="24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"></path></svg>';
    dcl.remove(dark);
  } else {
    lights.innerHTML = '<svg fill="none" viewBox="0 0 24 24" width="24" height="24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"></path></svg>';
    dcl.add(dark);
  }
}

document.addEventListener("DOMContentLoaded", function() {
  if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
    darkModeToggle();
  }
  var toc = document.getElementById("table-of-contents");
  if (toc !== null) {
    document.getElementById("postamble").appendChild(toc);
  }
}, false);
