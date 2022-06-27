# OM-pd 


*OM-pd* aims to put the PureData (mainly offline mode) inside OM-Sharp. It could help to work with process lot of samples using VST plugins, SoundFont, and general audio processing.


### Setting of Microtonal Player

You need to install Cyclone and Else by Alexandre Porres. Open PureData, go to `Help->Find External` then search for `else` and `cyclone`. Click in `INSTALL`.


<img src="https://github.com/charlesneimog/OM-pd/blob/master/resources/Using%20Deken.png" width="512"/>


### Linux PureData for Windows Users

It is a super-specific feature that will be few used. The main about this is to be possible to test some objects of PureData, developed in Unix Systems, without wasting time with the compilation for Windows OS.  
To work you need: 

1. Open `PowerShell` in Administrator and run `wsl --install`.
2. Then `wsl --install -d Ubuntu`. 
3. Then configure your username and password.
4. Then run `sudo apt-get install puredata`.
5. Now you need to compile you Linux object, open Linux PureData (use [GWSL](https://github.com/Opticos/GWSL-Source) if you want the GUI), make the patch. 

### Examples 


In my compositional uses, it was possible to make large spacialitation works, synthesis, and bigdata audio processing.


#### Using IEM Plugins and 3DC class

The patch processes a lot of audio data using tridimensional coordenates of 3DC. 

![IEM Plugins and OM-pd](https://github.com/charlesneimog/OM-pd/blob/master/resources/Exemplo%20-%20Espacializacao.png)

Please, use Headphones!! Real-time Examples...

https://user-images.githubusercontent.com/31707161/175837214-feb1fe11-1b7d-42de-a8da-8e2c594e79ce.mp4

#### Playing DataStream

The patch play `data-stream`. Here I import a tradicional score from my notation software (using om-py) and then play it using Orchidea-Sol samples, PureData.  

![Data-stream player](https://user-images.githubusercontent.com/31707161/175836639-b4bef184-53c4-4389-97e6-4cf1fb248cce.png)

https://user-images.githubusercontent.com/31707161/175836889-23bd7d57-a093-40b3-9fbd-bcdadac4b5dd.mp4


  
