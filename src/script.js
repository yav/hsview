$(document).ready(function() {
  var div = $('<div/>')
            .attr('id','controls')
            .text('Hide:')

  function addOpt(sels,lab) {
    var me = $('<div/>');
    var input = $('<input/>')
                .attr('type','checkbox')

    input.click(function() {
      var targets = $([]);
      jQuery.each(sels, function(ix,sel) { targets=targets.add(sel); });
      if (input.is(':checked')) targets.hide(); else targets.show();
    });
    div.append(me.append([input,lab]));
  }

  addOpt(['.import'],'imports');
  addOpt(['.code','.instance>.sub'],'code');
  addOpt(['.signature'],'signatures');
  addOpt(['.type'],'types');
  addOpt(['.class','.instance'],'classes and instances');
  addOpt(['.class>.sub', '.instance>.sub'], 'methods');
  addOpt(['.type>.sub'], 'constructors');

  $('body').append(div);
});


