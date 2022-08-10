$( document ).ready(function() {
 // ProgressBars bindings
Shiny.addCustomMessageHandler("update-diseaseIndicator", function(
  data
) {
  var id = data.id;
  var elVal = document.getElementById(id + "-value");
  var elLabel = document.getElementById(id + "-label");
  var value = data.value;

  elVal.style.width = value + "%";
  elVal.style.backgroundColor = data.colour
  
    if (data.label !== null) {
    elLabel.innerText = data.label;
  }
  
});
});
