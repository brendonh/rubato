#!/usr/bin/liquidsoap

set("log.stdout", true)
set("server.telnet", true)

add_protocol("rubato",
  fun (arg,delay) ->
    get_process_lines("echo '"^string_of(delay)^"' | nc localhost 1358" ))


output.shoutcast.mp3(
        host = "localhost", 
        port = 9183, 
        password = "br3ndc4st", 
        fallback([
                request.dynamic(
                        {request(get_process_output("nc localhost 1358" ))},
                        conservative=false,
                        timeout=3.),
                blank()
        ])
)
