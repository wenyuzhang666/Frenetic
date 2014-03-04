jot = require('json-over-tcp');

var applicationPort = 8099;

// Packs a dotted-decimal IP address into a 32bit integer
function packIP(ipStr) {
  var bytes = ipStr.split(".").map(function(x) { return Number(x); });
  var ip = 0;
  for (var i = 0; i < 4; i++) {
    ip += bytes[i] << ((3 - i) * 8);
  }
  return ip;
}

// Creates a policy that filters out all IP addresses in ipList
function blacklist(ipList) {
  var pol = false;
  for (var i = 0; i < ipList.length; i++) {
    pol = { or: [ { ip4Dst: packIP(ipList[i]) }, pol ] };
  }
  return { not: pol };
}

var cornellIPs = [
  "128.253.173.246", 
  "128.253.173.241", 
  "128.253.173.242", 
  "128.253.173.243", 
  "128.253.173.244", 
  "128.253.173.245"
];

var policy = { filter: blacklist(cornellIPs) };

var server = jot.createServer(applicationPort);

server.on('connection', function(socket){
  socket.on('data', function(data) {
    socket.write(policy);
  });
});

server.listen(applicationPort);
