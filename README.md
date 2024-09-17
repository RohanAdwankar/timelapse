# timelapse

https://github.com/user-attachments/assets/781ea125-625b-4531-b5e0-a0cb59196118

## Features
In Progress:
- Ensure succesful installs/builds of all commits possible
- Take screenshots of builds
- Combine screenshots into timelapse video
  
Completed:
- CLI to input Github URL and amount of commit to process
- Binary search of commit given Github URL
- Extract scripts from package.json to see changes over time
- Runs npm install for commits to see changes in packages over time.
- Runs npm dev for all commits with succesful installs
## Background
Goal of the project was to learn Haskell and the Node.js Package management system.
## How to Run
git clone https://github.com/RohanAdwankar/timelapse.git
stack build
stack exec timelapse
## How to Install Stack
### This will also instal GHC, the compiler for Haskell
curl -sSL https://get.haskellstack.org/ | sh
