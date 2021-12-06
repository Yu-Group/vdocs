$(document).ready(function() {
  // js function to add info button before collapsible info block
  $( ".help-info" ).each(function(index) {
    if ($('#help-btn-' + index).length == 0)
      $( this ).prev('p').append(
        ' <span class="help-btn" id="help-btn-' + index + '">&#9432;</span>'
      )
  })

  // js function to show collapsible info help text
  $('.help-btn').click(function(){
    $( this ).parent().next('.help-info').slideToggle('fast')
  });
});