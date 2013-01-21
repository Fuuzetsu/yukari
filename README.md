yukari
======

Beware
------
This piece of software is _not_ yet in an usable state. I mean this in a `it's missing nearly all the features and is not usable unless you change the source and recompile` and not in `uh oh, it's not pretty enough yet ;_;` way.

Most configuration is currently done in the source and the program is recompiled with each change. The aim is to provide command line control and config file(s). An easier to implement alternative would be to have a Settings module that the user is expected to use to provide the configuration for every setting in the program. The type system would be used to ensure that all settings provided. The user benefit of this is that the user is guaranteed not to have missed something and many of the settings are very unlikely to frequently change anyway. The downside is that binaries can no longer be distributed and that the user needs access to a Haskell compiler as well as having all the dependencies.

Tests and documentation should slowly be making their way into the program.

From here on, this program will either be referred to as `this software`, `this program`, `yukari`, `Yukari` or a derivative.

Overview
------
This software aims to help manage and automate usage of the AnimeBytes tracker.
This includes anything from spending your bonus points (yen) through fetching some torrents for a specific series to fetching all torrents with some more general property. See below for a more detailed list of features.

It should be mentioned that this software can _not_ be used to download any of the files itself, i.e. it's not a BitTorrent client. It is therefore up to the user to configure their client accordingly.

Lastly, this software is written for myself. This means that it will be heavily tailored towards my current setup as of now. I am aiming for a fairly generic program but if the program does something in the way that doesn't suit you, you are temporarily out of luck. An example would be the fact that currently mass torrent fetching uses a function that specifies where the torrent files should be saved. Personally, I use this to point the program to my rtorrent watch directories. If your torrent client expects different means of communication then you can't use this feature in the same way as I do.


This program is licensed under the GPLv3 (see LICENSE).

Table of features
------
See below for a table of features and their status. A brief explanation of each feature and its issues follows in the next section.
<table border="1">
<tr>
<th>Feature</th>
<th>Status</th>
</tr>
<tr>
<td>Spending yen</td>
<td>~</td>
</tr>
<tr>
<td>Sending yen to users</td>
<td>✗</td>
</tr>
<tr>
<td>Sending messages to users</td>
<td>✗</td>
</tr>
<tr>
<td>Reading mailbox messages</td>
<td>✗</td>
</tr>
<tr>
<td>Viewing basic user information</td>
<td>✗</td>
</tr>
<tr>
<td>Crawling regular torrent pages</td>
<td>✓</td>
</tr>
<tr>
<td>Crawling music pages</td>
<td>✗</td>
</tr>
<tr>
<td>Fetching generic torrents</td>
<td>✓</td>
</tr>
<tr>
<td>Fetching named torrents</td>
<td>✗</td>
</tr>
<tr>
<td>Custom torrent filtering</td>
<td>✓</td>
</tr>
<tr>
<td>Per torrent group preferences</td>
<td>✗</td>
</tr>
<tr>
<td>Inbox processing</td>
<td>?</td>
</tr>
<tr>
<td>Database storage</td>
<td>?</td>
</tr>
</table>

✓ = Done <br />
~ = Partial/In progress <br />
? = Possible future feature <br />
✗ = Not started/Completely broken <br />

Feature overview
------
Below is a description of the features mentioned in the previous section. This section aims to explain what the different features are and their current state. For the state of the program as a whole, see the next section. In places where I talk about supplying configuration/preferences, I mean the ability to do so, without regard on how it is actually done at this stage.
* Spending yen:
The program should be able to log in, go to the store and spend the yen on upload or donate to the freeleech pool. The user should be able to supply the minimum amount of yen they want to have left over. There is no plan to allow the user to spend the yen on anything else.
Currently the program is able to spend yen on upload but not on freeleech.

* Sending yen to users:
Yukari should be able to send a user-specified amount of yen to a specified user. The user should be able to pass in the minimum amount of yen left over, other person's username and the amount of yen to send, either with or without the site tax accounted for. The yen will not be sent if the user doesn't have enough or sending would go under the minimum amount specified.
This feature has not been started on.
* Sending messages to users:
Given a username, subject and text, yukari should be able to send that user a message with given information.
This feature has not been started on.
* Reading mailbox messages:
Given a number `n`, yukari should be able to display the nth message from the most recent one gotten. She should also be able to simply print inbox content with the messages number.
This feature has not been started on.
* Viewing basic user information:
Given a username, yukari should be able to fetch a user's info and output it to the terminal. The output will only contain the basic information such as the user's ration and torrent statistics. It should be able to handle users with hidden information.
This feature has not been started on.
* Crawling regular torrent pages:
Given a search page, yukari should be able to crawl through all of the results and categorise all seen torrents into appropriate data types. This feature is heavily used by other parts of the program.
This feature is finished.
* Crawling music pages:
Same as the `Crawling regular torrent pages` feature except for music torrents. This actually used to work transparently but there is now more parsing into types in place as well as more information being retrieved.
This feature is only partially implemented.
* Fetching generic torrents:
This feature will allow the user to fetch all torrents that fit user specified preferences. This essentially uses the `Custom torrent filtering` function on all torrents seen.
This feature is partially finished. For some preferences we can use the site's search to greatly narrow the search space.
* Fetching named torrents:
This feature is similar to `Fetching generic torrents`. The difference is that the torrent name setting is required. This allows to greatly narrow the number of torrents that have to be looked through thanks to the site's in-built search.
* Custom torrent filtering:
The program should be able to take user preferences which will then be used to filter the torrents obtained from other parts of the program. The filtering should work on virtually all types of information that a torrent data type holds.
This feature is finished.
* Per torrent group preferences:
In conjunction with `Fetching named torrents` feature, this will allow the user to specify a preferred torrent from a group. This means that only a single torrent from a group is picked. The best torrent is decided based on user provided preferences. The user should also be able to override the `single torrent` restriction and specify either the number of best torrents, all torrents that fit the preference, or both.
This feature has not been started on.
* Inbox processing:
This feature aims to process some of the stock System messages and act upon them accordingly. Given a `torrent removed` message, the program could be set up so that it stops the torrent and removes it from the torrent client. This is however difficult to do. Many torrent clients don't have means of such communication, and those that do aren't very easy to talk either. Furthermore, the program would have to auto-magically know which torrent on the site responds to which torrent locally and in the torrent client. There would have to be a database involved and set up on the torrent client side. For these reasons, it's unlikely that this feature will be implemented.
* Database storage:
This feature would let the program store information in local database. This would involve all the information about every torrent seen and fetched. The advantages:
+ Don't have to parse all torrents on the site every time we want to do a massive fetch and we don't care about getting every single torrent
+ Can readily skip torrents we have already fetched in the past
+ Can effectively browse all torrents on the site without even going online
+ Fast, local searches and filters
+ AnimeBytes no longer *shakes fist* at this program's users for unusual bandwidth usage (although this is mitigated by superior (to current) search based on user preference with combination of site search, some things can't be searched for using the site (such as specific number of seeders etc.))
There are however fair few disadvantages to this:
+ The database would be fairly large. We would be storing quite a lot of information for each of the ~37000 torrents.
+ The database would have to be updated every now and again. This means going through the couple of hundred of pages every time we want to have a more recent database as we have to be able to remove no longer existing torrents and update all the torrents we currently have stored as well as adding any new ones. This is more of a common courtesy worry in regards to hitting the AB servers. I don't believe it is a big issue unless many, many people do this on frequent basis. It's pretty much the same issue that Linux package repositories have and it seems to work quite well for them.
+ Storing whether we have fetched a torrent is a one way operation only. The program can fetch the torrent and mark it as such in the database, but the torrent client can't do the reverse when it deletes the torrent meaning that we might an up with some incorrect values. An alternative to not mark the torrent as fetched at all. The download operation will skip the torrent if it already exists in the download destination and doesn't download it if it is. This is therefore only an issue if the user wants to see exactly what torrents from AB he/she already has. All in all, I think I will include this but won't actually use this value to decide anything in the program.

There is also another thing to consider. All torrent downloads can be done without logging in. What we do need however is our site torrent passkey which we are given and is constant (unless it expires due to inactivity but in that case you have bigger issues) and an authkey which I'm not sure about. It seems to never change but if it did then none of our download links in the database would work. Updating them would be fairly trivial (log in, have a look at any torrent to get an authkey and update all database entries) but a nuisance none the less. I am thinking about allowing the user to fetch every single torrent on the site and storing it locally. The database would record the directory of storage and will either remember the full path or just the root as it should be able to figure out its own naming scheme. The advantage of this is that we don't have to worry about download links expiring and fetching some torrents is as simple as having the program copy some torrent files into different directories as normal. We simply skip the download step. The trade off here is that we have to fetch the torrents first. All the torrent would easily take hundreds of MB (possibly going into GB range) meaning that it could be a large hit to the server bandwidth if many people did this. On an upside, this means that the user never has to download any of these again. Clearly, we still use more bandwidth downloading every single torrent than we would have normally spent on downloading torrents. Quickly doing some arithmetic, I get that to traverse every single page with torrents (torrents.php and torrents2.php) takes about 15MB of bandwidth. This means that it's pretty unreasonable to expect that the bandwidth saved not visiting the site will offset the large amount of data we would have to download to do so. Possibly some kind of hybrid solution would be good for this, where we only archive certain torrents and still have to fetch the others.

For all of the above reasons, I'm unsure whether a database should be put in place. I would like one but it would probably not involve downloading all the torrents to a local store.


General issues with the program
------
This section will briefly describe what needs to be done with the program as a whole.
* log-in verification -- the program doesn't test for successful log-in which results in a parsing exception later
* proper exception handling throughout the code -- currently the program exits on a failed download
* more robust parsing -- given an incorrect page, the program will simply crash when the parser fails to read something; it would be more favourable to skip the page (while notifying the user) rather than have it abort in the middle of a large fetch job; given a torrent with data that was not accounted for at compile time will result in similar behaviour; I can't ensure that AnimeBytes doesn't change the formatting on the site but I should at least be able to be prepared for it instead of finding out the hard way
* work out the exact settings I want to let the user change and how to let them change it -- e.g. simply letting the user pass in [CurlOptions] vs just letting them pass some data that we can then use as argument to curl (such as up/down speed)
* generalise the program structure -- currently I have types that encapsulate types and so on and then functions that tend to work on the top type; the issue begins when I want to change the top type a bit to accommodate some changes; perhaps use of type classes would be more appropriate; this also encapsulates the ability to freely swap out parsers - we could even simply swap out a parser and parse a whole different site with the rest of the program being the same
* work out how to let the user interact with the program -- command line input will be used but with such a wide variety of settings and options, I have to compromise for some settings being already stored somewhere and the command line options simply telling the program what to do and where to look; ideally the user should be able to change any setting on the command line but it would quickly turn into parsing hell that way
* documentation and testing -- self explanatory; currently it's just a mess of code hastily split into few modules when Main got too big to find anything
* general clean-up -- to be done when (nearly) everything is already in place (especially generalisation)
