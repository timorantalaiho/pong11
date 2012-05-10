require 'socket'
require 'json'
require 'sinatra/base'
require 'sinatra/json'

class ClientQuitError < RuntimeError; end

port = ARGV.shift || 0 # default is to use the next available port
host = ARGV.shift # default is to bind everything

server = host ? TCPServer.open(host, port) : TCPServer.open(port)

port = server.addr[1]
addrs = server.addr[2..-1].uniq

puts "*** listening on #{addrs.collect{|a|"#{a}:#{port}"}.join(' ')}"

loop do
  socket = server.accept

  Thread.start do # one thread per client
    s = socket

    port = s.peeraddr[1]
    name = s.peeraddr[2]
    addr = s.peeraddr[3]

    puts "*** recieving from #{name}:#{port}"
    puts "Lapna"

    begin
      ballX = 291.0
      while line = s.gets # read a line at a time
        raise ClientQuitError if line =~ /^die\r?$/
        puts "#{addr} [#{Time.now}]: #{line}"
        msg = JSON.parse(line)
        if msg['msgType'] == 'join'
          # Start test1
          times = [0, 25, 50]
          times.each do |time|
            s.puts('{"msgType": "gameIsOn", "data": {"time": #{time}, "left": {"y": 186.0, "playerName": "hevonen"}, "right": {"y":310.0, "playerName":"pong11"}, "ball": {"pos": {"x":291.0, "y":82.0}}, "conf": {"maxWidth":640, "maxHeight":480, "paddleHeight":50, "paddleWidth":10, "ballRadius":5, "tickInterval":15}}}')            
          end
        end
      end

    rescue ClientQuitError
      puts "*** #{name}:#{port} disconnected"

    ensure
      s.close # close socket on error
    end

    puts "*** done with #{name}:#{port}"
  end

end
