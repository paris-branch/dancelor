function hideMenu() {
  var menu = document.getElementById("nav");
  menu.style.display = "none";
}
window.addEventListener('load', hideMenu, false);

function toggleMenu() {
  var menu = document.getElementById("nav");
  if (menu.style.display == "none") {
    menu.style.display = "block";
  } else {
    menu.style.display = "none";
  }
  return;
}
