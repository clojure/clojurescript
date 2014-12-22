var url = 'http://localhost:9000/'
var page = require('webpage').create();
page.open(url, function (status) {
  console.log("[phantomjs load] " + url);

  page.onConsoleMessage = function(msg) {
    console.log("[phantomjs console] " + msg);
  };

  page.onError = function(msg, trace) {
    console.error("[phantomjs error] " + msg + "\n" + trace);
  }

  page.onClosing = function(closingPage) {
    console.log("[phantomjs exit]");
    phantom.exit(0);
  };
});
