# racket-camstream

A Racket [camdisplay](https://github.com/tonyg/camstream#readme)
implementation compatible with the original.

Displays streaming video from one or more webcams, using just the
built-in Racket GUI libraries for displaying bitmaps.

A separate capture program (e.g. the one included with the Java
version of camstream) injects frames into a named AMQP fanout exchange
on a RabbitMQ instance, and this program retrieves and displays the
frame stream by using the STOMP protocol to connect to the RabbitMQ
server.

## Copyright and License

`racket-camstream` is [open-source](http://www.opensource.org/)
software, licensed under the very liberal [MIT
license](http://www.opensource.org/licenses/mit-license.php):

    Copyright (c) 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
