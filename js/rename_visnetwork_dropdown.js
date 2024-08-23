setTimeout(function(){
  var optionElement1 = document.querySelector('#nodeSelectindikator_auswahl-network option[value=""]');
  var optionElement2 = document.querySelector('#selectedByindikator_auswahl-network option[value=""]');
  optionElement1.textContent = 'Wähle ein Thema';
  optionElement2.textContent = 'Wähle ein Handlungsfeld';
}, 1000);
