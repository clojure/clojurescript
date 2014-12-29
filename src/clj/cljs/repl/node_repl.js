process.env.NODE_DISABLE_COLORS = true;

var net = require("net");

net.createServer(function (socket) {
    var buffer = "", ret;
    socket.setEncoding("utf8");

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
                    ret = process.binding('evals').NodeScript.runInThisContext.call(
                           global, data, "repl");
                } catch (x) {
                    console.log(x.stack);
                }
            }
            // TODO: can we just console.log? - David
            if(ret !== undefined && ret !== null) {
                socket.write(ret.toString());        
            } else {
                socket.write("nil");
            }
            socket.write("\0");
        }
    });

}).listen(5001);

console.log("ClojureScript Node.js REPL server listening on 5001")
