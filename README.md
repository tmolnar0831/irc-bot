# Photter BOT

Photter is a small Common Lisp IRC BOT.

## BOT commands

### .weather

This is the main, currently most useful command of the bot. It accepts a single city name like

`.weather barcelona`

Or a city name followed by a two digit country code like

`.weather madrid es`

It is possible to use state or province name after the city as well like

`.weather millinocket maine`

It is also possible to use multi word city names, in this case the **country code or state name** is **mandatory**.

`.weather smiths falls ontario`

Calling the command without any parameter will order the BOT to look up the location data in its database for the invoking nickname if it previously set the location data with `.setlocation`.

### .setlocation

Setting the location for a nickname means that the `.weather` command will look up the saved location from a database instead of from its parameters.

Running this command again will **update** the location setting.

`.setlocation budapest hu`

### .getlocation

This command does not accept any parameters, it returns the saved location for the invoking nickname, echoing in the channel.

### .remlocation

This command removes the location from the database for the invoking nickname.

### .version

Echoes the BOT version in the channel.

### .help

Echoes a useful help information in private.

## Installation and usage

See in the [Wiki](https://github.com/tmolnar0831/photter/wiki).

## License

Copyright (c) 2018-2019. Tamas Molnar

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
