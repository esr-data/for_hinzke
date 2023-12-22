shinyjs.init = function() {
  $('body').on('click', '.side_circle', function(ev) {
    Shiny.setInputValue('monitor-mon', ev.target.id, {priority: 'event'});
  });
};
