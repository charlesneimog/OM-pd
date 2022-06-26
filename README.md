# OM-pd 


*OM-pd* aims to put the PureData (mainly offline mode) inside OM-Sharp. It could help to work with VST plugins, SoundFont, and general audio processing, among other things.  


### Setting of Microtonal Player

It should work automatically in macOS and Windows, if not please let me know. In Linux, you need to install Cyclone and Else by Alexandre Porres.

### Linux PureData for Windows Users

It is a super-specific feature that will be few uses. The main about this is to be possible to test some objects of PureData, developed in Unix Systems, without wasting time with the compilation for Windows OS.  
To work you need: 

1. Open `PowerShell` in Administrator and run `wsl --install`.
2. Then `wsl --install -d Ubuntu`. 
3. Then configure your username and password.
4. Then run `sudo apt-get install puredata`.
5. Now you need to compile you Linux object, open Linux PureData (use [GWSL](https://github.com/Opticos/GWSL-Source) if you want the GUI), make the patch. 

### Examples 


In my compositional uses, it was possible to make large spacialitation works, synthesis, and bigdata audio processing.


#### Using IEM Plugins and 3DC class

![IEM Plugins and OM-pd]([http://url/to/img.png](https://github.com/charlesneimog/OM-pd/raw/master/resources/Exemplo%20-%20Espacializacao.png
)
