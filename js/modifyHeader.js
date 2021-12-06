$(document).ready(function() {
  // js function to modify setup

  // modify header
  $(".header-panel .col-xs-3").removeClass("col-xs-3").addClass("col-xs-12")

  // modify "Code" button
  $(".btn-toolbar>.btn-group>a>span:first").html('&#9881;')
  $(".btn-toolbar>.btn-group>ul").addClass('pull-right')
  $(".btn-toolbar .caret").remove();
  
  // add show and hide all info button
  var settings_item = $(".btn-toolbar>.btn-group>ul:first>.divider") 
  settings_item.after(
    '<li><a id="show-all-info" href="#">Show All Tips</a></li>',
    '<li><a id="hide-all-info" href="#">Hide All Tips</a></li>',
    '<li role="separator" class="divider"></li>'
  )
  $('#show-all-info').click(function(){
    $(".help-info").show()
  });
  $('#hide-all-info').click(function(){
    $(".help-info").hide()
  })

  // add clear cache button 
  var settings_item = $(".btn-toolbar>.btn-group>ul:first>.divider:last") 
  settings_item.after(
    '<li><a id="clear-cache" href="#">Clear Cache</a></li>',
    '<li role="separator" class="divider"></li>'
  )
  $('#clear-cache').click(function(){
    localStorage.clear()
  });
});