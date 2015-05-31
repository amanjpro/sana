# Sana Compiler Framework

[![Build Status](https://travis-ci.org/amanjpro/sana.svg?branch=master)](https://travis-ci.org/amanjpro/sana)

Sana is an extensible compiler framework written in Scala. As a research
project, Sana aims to be both easy-to-use and safe. Sana's design has a number
of considerations, the framework heavily uses monads, cake-pattern and
immutability everywhere.



## Installation

To play with Sana, first clone the project:
```
git clone github.com/amanjpro/sana
```

Then, using `sbt` you can build the wanted target. For example to build the
`tiny` language, you do:

```
sbt tiny/compile
```

Please consult the project's [wiki](https://github.com/amanjpro/sana/wiki) 
to know all possible targets.

You can generate a fat jar by running assembly for your target, for example for
`tiny` it will be:

```
sbt tiny/assembly
```


## Usage

Please refer to the [wiki](https://github.com/amanjpro/sana/wiki)

## Contributing

1. Fork it!
2. Experiment with the plugin system of Sana
3. Play with Sana more and implement a language feature on top of an
   existing language.
4. Go through [wiki](https://github.com/amanjpro/sana/wiki/Contributing)
5. Report bugs

[](## Q/A Forum)

[](## History)


[](## Credits)

## License

```
Copyright (c) <2015>, Amanj Sherwany - http://www.amanj.me
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
