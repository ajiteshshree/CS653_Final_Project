
<h1>Haskell Chat Application</h1>
<p>
    A real-time chat server built with Haskell using WebSockets and a lightweight HTTP server. 
    Users can join chat rooms, send messages, and see the list of active users in the room. 
    This application works only on a Local Area Network (LAN) where all users share the same Wi-Fi connection.
</p>

<h2>Features</h2>
<ul>
    <li>Real-time communication using WebSockets.</li>
    <li>Join or leave chat rooms dynamically.</li>
    <li>Broadcast messages to all users in a room.</li>
    <li>See a live list of users in the room.</li>
</ul>

<h2>Project Structure</h2>
<ul>
    <li><code>Main.hs</code>: The Haskell server code.</li>
    <li><code>app.js</code>: Client-side JavaScript for handling WebSocket communication.</li>
    <li><code>index.html</code>: The main UI for the chat application.</li>
    <li><code>styles.css</code>: Styling for the chat app.</li>
    <li><code>chat-app.cabal</code>: Haskell project configuration.</li>
</ul>

<h2>Main.hs Function Overview</h2>
<p>The <code>Main.hs</code> file is the core of the chat application. It implements the server logic using Haskell’s concurrency model and WebSockets. Below is a detailed explanation of its functionality:</p>

<ul>
    <li><strong>Data Types:</strong>
        <ul>
            <li><code>ClientMessage</code>: Represents messages from the client (join room, leave room, send chat).</li>
            <li><code>ServerMessage</code>: Represents messages sent by the server to the client (user chat messages).</li>
        </ul>
    </li>
    <li><strong>Server State:</strong>
        <ul>
            <li>Tracks all active rooms and users in a thread-safe way using STM (Software Transactional Memory).</li>
        </ul>
    </li>
    <li><strong>Initialization:</strong>
        <ul>
            <li><code>initServerState</code>: Creates an empty server state for managing rooms and users.</li>
            <li><code>main</code>: Starts the server and listens on port <code>8080</code>.</li>
        </ul>
    </li>
    <li><strong>WebSocket Handling:</strong>
        <ul>
            <li><code>wsApp</code>: Accepts WebSocket connections and starts handling communication with clients.</li>
            <li><code>pingClients</code>: Sends periodic pings to keep the connection alive.</li>
        </ul>
    </li>
    <li><strong>Room Management:</strong>
        <ul>
            <li><code>joinRoom</code>: Adds a user to a specific room. Creates the room if it doesn’t exist.</li>
            <li><code>leaveRoom</code>: Removes a user from a room. Deletes the room if it becomes empty.</li>
        </ul>
    </li>
    <li><strong>Message Handling:</strong>
        <ul>
            <li><code>sendMessage</code>: Broadcasts a user’s message to everyone in the room.</li>
            <li><code>broadcastUserList</code>: Sends an updated list of users in a room to all connected clients.</li>
        </ul>
    </li>
</ul>

<p>This file demonstrates how Haskell's lightweight threads and STM make it easy to manage concurrent WebSocket connections and shared state in real-time applications.</p>


<h2>Requirements</h2>
<h3>Server</h3>
<ul>
    <li>GHC (Glasgow Haskell Compiler)</li>
    <li>Cabal (Build system)</li>
</ul>

<h3>Client</h3>
<ul>
    <li>Any modern web browser.</li>
</ul>

<h2>Setup</h2>
<h3>Server</h3>
<ol>
    <li>Install dependencies:
        <pre>cabal update</pre>
        <pre>cabal install</pre>
    </li>
    <li>Run the server:
        <pre>cabal build</pre>
        <pre>cabal run chat-app</pre>
    </li>
    <li>The server will start on <code>http://[server-ip]:8080</code>. 
        Replace <code>[server-ip]</code> with your server machine's local IP address (e.g., <code>192.168.0.100</code>).</li>
</ol>

<h3>Client</h3>
<ol>
    <li>Open <code>index.html</code> in a browser.</li>
    <li>Enter a username and room name to join a chat.</li>
    <li>Share your machine's local IP and port with others on the same LAN to enable them to join.</li>
</ol>

<h2>Demonstration</h2>
<p>Below are two images demonstrating the chat application:</p>
<h3>Login Screen</h3>
<img src="demo\startPage.png" alt="Login Screen Demo" style="max-width: 100%; height: auto;">
<h3>Chat Room</h3>
<img src="demo\mainPage.png" alt="Chat Room Demo" style="max-width: 100%; height: auto;">

<h2>File Overview</h2>
<ul>
    <li><code>Main.hs</code>: Core server logic. Handles WebSocket connections and message routing.</li>
    <li><code>app.js</code>: Connects to the WebSocket server. Sends user actions (join, leave, chat) to the server. Updates the UI with messages and user lists.</li>
    <li><code>index.html</code>: Provides the user interface for the chat application.</li>
    <li><code>styles.css</code>: Styles the login screen and chat UI.</li>
</ul>

<h2>How to Use</h2>
<ol>
    <li>Start the server.</li>
    <li>Open the client UI (<code>http://[server-ip]:8080</code>).</li>
    <li>Enter a username and room name to join a chat.</li>
    <li>Share your IP address with others on the same LAN so they can join.</li>
    <li>Chat with others in the same room.</li>
</ol>

<h2>Author</h2>
<p>
    <strong>Ajitesh Shree</strong><br>
    Email: <a href="mailto:ajitesh2021@gmail.com">ajitesh2021@gmail.com</a>
</p>

<h2>License</h2>
<p>This project is licensed under the BSD-2-Clause License.</p>

