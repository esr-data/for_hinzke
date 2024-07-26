function selectDataset(input) {

    var currentValue = Shiny.shinyapp.$inputValues["datensaetze-select_dataset"];
    var newValue = input;

    if (currentValue === undefined) {
      currentValue = -1;
    }

    if (newValue == currentValue){
      newValue = newValue * -1;
    }
    Shiny.onInputChange('datensaetze-select_dataset', newValue);
}
