function goBackPage() {
  var currentValue = Shiny.shinyapp.$inputValues.geh_zurueck;
  if (currentValue === undefined) {
    currentValue = 1;
  }
  var newValue = currentValue * -1;
  Shiny.onInputChange('geh_zurueck', newValue);
}
