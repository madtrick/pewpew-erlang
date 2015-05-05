[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/avioneta-server/README)](https://github.com/igrigorik/ga-beacon)

#AVIONETA-SERVER

Backend for the game [Avioneta](https://madtrick.github.io/avioneta)

##About the game

Avioneta is a multiplayer game where players fight to destroy its opponents. The game runs in the browser using HTML5 technologies like WebSockets and Canvas.

The game is architectured in a client-server fashion where the [clients](https://github.com/madtrick/avioneta) send commands to the server to update the game state.

The code for the client of the game is on this other repository: [https://github.com/madtrick/avioneta](https://github.com/madtrick/avioneta)

##About the server
Its main goals are:

  * Maintain and update game state according to player actions.
  * Offer a WebSocket endpoint for its clients.

Clients connect to the server opening a WebSocket connection. Using this connection they send commands which are processed by the server to update game state and if necesary:

  * Reply to the origin of the command.
  * Notify other clients the changes in game state.
  
  
### Limitations
At this moment, the server have several limitiations:

* It is not authoritative. It will accept any command from the clients. This opens the door to cheaters.
* It can only handle one game at a time. Clients can not create separate (i.e. private) games.

## Author

Server developed by Farruco Sanjurjo. You can find me:

 * On Twitter [@madtrick](https://twitter.com/madtrick)
 * Email madtrick@gmail.com
 
##License
Copyright [2013] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
