# airflow-materials
Materials for the course: The Hands-On Guide

# Setting up GCP compute engine
You should have received an ip address 172.16.7.*** from cloud ops with a username and password. 

# On your Amazon Workspace 
1) Install VScode and Git for Windows

2) Open VScode and install the Remote SSH extension
![Screen Shot 2021-07-06 at 12 35 13 PM](https://code.savvysherpa.com/storage/user/296/files/f7bf5f80-de56-11eb-8a26-0948f20c9b98)

3) Open the Command Palette in VScode using Ctrl+Shift+P
![Screen Shot 2021-07-06 at 12 38 36 PM](https://code.savvysherpa.com/storage/user/296/files/42d97280-de57-11eb-85c8-22baad4febb0)

4) Search for and select "Remote SSH: Add New SSH Host" ![Screen Shot 2021-07-06 at 12 41 06 PM](https://code.savvysherpa.com/storage/user/296/files/8c29c200-de57-11eb-8c8c-0b75a1625e53)

5) Login with your user name and IP address
![Screen Shot 2021-07-06 at 12 43 02 PM](https://code.savvysherpa.com/storage/user/296/files/c09d7e00-de57-11eb-9e3e-d5a555411647)

6) Click any config file to save the information

7) Open the Command Palette again using Ctrl+Shift+P

8) Search for and select SSH: Connect to Host

9) Choose your IP address and use the password given to you by cloud ops.

10) Open a terminal and change your password using `passwd`

11) Click 'Open Folder' to open your user directory, enter your password when requested
![Screen Shot 2021-07-06 at 12 48 38 PM](https://code.savvysherpa.com/storage/user/296/files/97c9b880-de58-11eb-8150-342bb19636bc)

12) Clone this repository into your user directory using 
``` 
git clone https://code.savvysherpa.com/hbrantley/udemy_hands_on_airflow_training.git
```

13) Hover your mouse next to your user name to refresh the file viewer and check that the training materials are there. 
![Screen Shot 2021-07-06 at 12 52 12 PM](https://code.savvysherpa.com/storage/user/296/files/335b2900-de59-11eb-8736-f70b7402fd85)

14) Open a terminal and install Docker by following the "Install using the repository" section here: https://docs.docker.com/engine/install/ubuntu/

15) Install docker composer for Linux by following these instructions: https://docs.docker.com/compose/install/

16) Create a docker group so you don't need to use `sudo` by following: https://docs.docker.com/engine/install/linux-postinstall/

17) `cd` into the section you want to run and start docker by running
```
./start.sh
```

18) To see the UI from your workspace browser you will need to use your VM's IP address `172.16.7.***:8080` instead of `localhost:8080`
