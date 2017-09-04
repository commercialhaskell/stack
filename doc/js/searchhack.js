// Please see https://github.com/commercialhaskell/stack/issues/3376

(function () {
  $(document).ready(function () {
    fixSearch();
  });

  function fixSearch() {
    var target = document.getElementById('rtd-search-form');
    var config = {attributes: true, childList: true};

    var observer = new MutationObserver(function(mutations) {
      observer.disconnect();
      var form = $('#rtd-search-form');
      form.empty();
      form.attr('action', 'https://' + window.location.hostname + '/' + window.location.pathname.split('/')[1] + '/' + window.location.pathname.split('/')[2] + '/search.html');
      $('<input>').attr({
        type: "text",
        name: "q",
        placeholder: "Search docs"
      }).appendTo(form);
    });

    if (window.location.origin.indexOf('readthedocs') > -1 || window.location.origin.indexOf('docs.haskellstack') > -1) {
      observer.observe(target, config);
    }
  }

})();
