function removeAndReplaceDefaultCSS() {
  var links = document.getElementsByTagName('link');
  for (var i = 0; i < links.length; i++) {
    if (links[i].href === 'http://passthrough.fw-notify.net/static/default.css') {
      console.log('Removing: ' + links[i].href);
      links[i].parentNode.removeChild(links[i]);
      var link = document.createElement('link');
      link.rel = 'stylesheet';
      link.href = 'default.css';
      document.head.appendChild(link);
    }
  }
}

document.addEventListener('DOMContentLoaded', function() {
  removeAndReplaceDefaultCSS();

  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      removeAndReplaceDefaultCSS();
    });
  });

  observer.observe(document.body, {
    childList: true, subtree: true
  });
});
