code assign s8  var x%%:=1  var y%%:=-1  var z:=xendcodevartype z Int8varvalue x 1varvalue y -1varvalue z 1code assign s8 to u8  var x%%:=100  var z##:=xendcodevarvalue z 100code assign s8 to u8 range  var x%%:=-100  var z##:=xendcoderuntime rangecode assign s8 to s16  var a%%:=10  var x%:=a  var b%%:=-128  var y%:=bendcodevarvalue x 10varvalue y -128code assign s8 to u16  var a%%:=10  var x#:=aendcodevarvalue x 10code assign s8 to u16 range  var b%%:=-128  var y#:=bendcoderuntime rangecode assign u8 to s8  var a##:=10  var x%%:=aendcodevarvalue x 10code assign u8 to s8 range  var b##:=200  var y%%:=bendcoderuntime rangecode assign u8 to x16  var a##:=127  var b##:=255  var i#:=a  var j#:=b  var k%:=a  var l%:=bendcodevarvalue i 127varvalue j 255varvalue k 127varvalue l 255code assign S16 to S8  var a%:=-100  var b%:=127  var j%%:=a  var k%%:=bendcodevarvalue j -100varvalue k 127code assign S16 to S8 range 1  var a% := 500  var i%%:=aendcoderuntime rangecode assign S16 to S8 range 2  var a% := -500  var i%% := aendcoderuntime rangecode assign S16 to U8  var a%:=127  var j##:=aendcodevarvalue j 127code assign S16 to U8 range 1  var a% := -127  var i##:=aendcoderuntime rangecode assign S16 to U8 range 2  var a% := -32000  var i## := aendcoderuntime rangecode assign S16 to U8 range 3  var a% := 32000  var i## := aendcoderuntime rangecode assign S16 to U16  var a%:=127  var b%:=31000  var j#:=a  var k#:=bendcodevarvalue j 127varvalue k 31000code assign S16 to U16 range 1  var a% := -127  var i#:=aendcoderuntime rangecode assign S16 to U8 range 2  var a% := -32000  var i# := aendcoderuntime rangecode assign U16 to S8  var a#:=127  var j%%:=aendcodevarvalue j 127code assign U16 to S8 range 1  var a# := 128  var i%%:=aendcoderuntime rangecode assign U16 to S8 range 2  var a# := 40000  var i%% := aendcoderuntime rangecode assign U16 to U8  var a#:=127  var b#:=255  var j##:=a  var k##:=bendcodevarvalue j 127varvalue k 255code assign U16 to U8 range 1  var a# := 500  var i##:=aendcoderuntime rangecode assign U16 to S16  var a#:=127  var b#:=255  var c#:=30000  var j%:=a  var k%:=b  var l%:=cendcodevarvalue j 127varvalue k 255varvalue l 30000code assign U16 to S16 range 1  var a# := 32768  var i%:=aendcoderuntime range