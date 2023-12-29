$(document).ready(function() {
  $('.collapsible-header').click(function() {
    $(this).next('.collapsible-content').slideToggle();
  });
});
