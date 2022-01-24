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

  // read in saved checkbox selections
  var checkboxValues = getCheckboxResponses();
  $.each(checkboxValues, function(key, value) {
    $("#" + key).prop('checked', value);
  });
});