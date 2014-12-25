process.env.NODE_DISABLE_COLORS = true;

var net     = require("net"),
    repl    = require("repl"),
    vm      = require("vm"),
    context = vm.createContext();

context.require = require;

net.createServer(function (socket) {
    var buffer = "", ret;
    socket.setEncoding("utf8");

    // redefine console.log
    context.node_repl_print = function(x) {
        ret = vm.runInContext(x, context, "repl");
        socket.write(ret.toString());
        socket.write("\1");
    };

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
                    ret = vm.runInContext(data, context, "repl");
                } catch (x) {
                    console.log(x.stack);
                    socket.write(x.stack+"\n");
                }
            }
            if(ret !== undefined && ret !== null) {
                socket.write(ret.toString());        
            } else {
                socket.write("nil");
            }
            socket.write("\0");
        }
    });

}).listen(5001);

console.log("repl.js listening on 5001")
