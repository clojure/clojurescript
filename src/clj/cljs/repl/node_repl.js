process.env.NODE_DISABLE_COLORS = true;

var net  = require("net");
var vm   = require("vm");
var dom  = require("domain").create();
var PORT = 5001;

try {
    require("source-map-support").install();
} catch(err) {
}

net.createServer(function (socket) {
    var buffer = "",
        ret    = null,
        err    = null;

    socket.setEncoding("utf8");

    dom.on("error", function(ue) {
        console.error(ue.stack);
    });

    socket.on("data", function(data) {
        if(data[data.length-1] != "\0") {
            buffer += data;
        } else {
            if(buffer.length > 0) {
                data = buffer + data;
                buffer = "";
            }

            if(data) {
                // not sure how \0's are getting through - David
                data = data.replace(/\0/g, "");
                try {
                    dom.run(function() {
                        ret = vm.runInThisContext(data, "repl");
                    });
                } catch (e) {
                    err = e;
                }
            }

            if(err) {
                socket.write(JSON.stringify({
                    status: "exception",
                    value: err.stack
                }));
            } else if(ret !== undefined && ret !== null) {
                socket.write(JSON.stringify({
                    status: "success",
                    value: ret.toString()
                }));
            } else {
                socket.write(JSON.stringify({
                    status: "success",
                    value: null
                }));
            }

            ret = null;
            err = null;

            socket.write("\0");
        }
    });

}).listen(PORT);

console.log("ClojureScript Node.js REPL server listening on", PORT);
