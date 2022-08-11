$( document ).ready(function() {
 // ProgressBars bindings
Shiny.addCustomMessageHandler("update-diseaseIndicator", function(
  data
) {
  const id = data.id;
  const value = data.value;
  const elementValue = document.getElementById(id + "-value");
  const elementLabel = document.getElementById(id + "-label");

  elementValue.style.width = value + "%";
  elementValue.style.backgroundColor = data.colour;
  
  if (data.label !== null) {
    elementLabel.innerText = data.label;
  }
  
});
});
