$(document).ready(function() {
  var div = $('<div/>')
            .attr('id','controls')
            .text('Hide:')

  function disappear(x) { x.fadeOut(); }
  function appear(x) { x.fadeIn(); }

  function addOpt(targets,lab) {
    var me      = $('<div/>');
    var input   = $('<input/>').attr('type','checkbox')

    input.click(function() {
      if (input.is(':checked')) appear(targets); else disappear(targets);
    });
    div.append(me.append([input,lab]));
  }

  function addRadio(targets,butNot,lab) {
    var me    = $('<div/>');
    var input = $('<input/>').attr('type','radio').attr('name','view');
    input.click(function() {
      disappear($('div:not(#controls, #controls *)'))
      appear(targets);
      disappear(butNot);
    });
    div.append(me.append([input,lab]));
  }

  function withHaddocks(xs) {
    return xs.add(xs.prev('.haddock_next')).add(xs.next('.haddock_prev'));
  }



  addRadio( $('.bcomment,.type,.signature,.class,.instance')
          , $('.instance>.sub')
          , 'Summary'
          );

  addRadio ( withHaddocks($('.type')), $('.type.sub'), 'Types');
  addRadio ( $('.class'),$([]), 'Classes');

  addRadio ( withHaddocks($('.signature')), $([]), 'Functions');

/*
  addOpt($('.comment'),'comments');
  addOpt($('.pragma'),'pragmas');
  addOpt($('.import'),'imports');
  addOpt($('.code,.instance>.sub'),'code');
  addOpt($('.signature'),'signatures');
  addOpt($('.type'),'types');
  addOpt($('.class,.instance'),'classes and instances');
  addOpt($('.class>.sub,.instance>.sub'), 'methods');
  addOpt($('.type>.sub'), 'constructors');
*/
  $('body').append(div);
});


