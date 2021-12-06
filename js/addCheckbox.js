$(document).ready(function() {
  $(".section>p").each(function(index) {
    if ($('#custom-checkbox-' + index).length == 0)
      if (($( this ).text() != "") && (!$( this ).text().match("^TODO")))
        $( this ).wrapInner(
          '<span>'
        ).prepend(
          '<label for="custom-checkbox-' + index + '">'
        ).prepend(
          '<input type="checkbox" id="custom-checkbox-' + index + '">'
        )
  })

  // save checkbox selections
  var checkboxValues = JSON.parse(localStorage.getItem('checkboxValues')) || {};
  var $checkboxes = $(":checkbox");
  $checkboxes.on("change", function(){
    $checkboxes.each(function(){
      checkboxValues[this.id] = this.checked;
    });
    localStorage.setItem("checkboxValues", JSON.stringify(checkboxValues));
  });
  $.each(checkboxValues, function(key, value) {
    $("#" + key).prop('checked', value);
  });
});