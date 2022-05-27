# README

This file provides a guide on:

1.  Setting up your your GitHub Desktop and pull the Dashboard Application into your local drive.
2.  Navigating the Dashboard
3.  Navigating the files underlying the application

### Pulling and updating the application to your local drive

**Step 1. Download GitHub Desktop**

The GitHub Desktop provides an easy method of pulling and updating the Dashboard onto your local drive. Go to <https://desktop.github.com/> and follow the download prompts. This should download an application to your desktop, open this. You will need a GitHub account to continue.

**Step 2. Cloning the repository onto your local drive**

To get the files in the GitHub onto your local drive, click on "*Clone a repository from the internet...*". Alternatively you can click the right hand side filter and choose the Dashboard repository and follow the prompts.

<img src="www/ReadME/1.Clone%20repo.PNG" width="580"/>

If you do not have the Dashboard repository in your list of existing repositories, go to the *URL* tab and paste in the Dashboard link (<https://github.com/daniel-crook/scenario_dashboard>).

Next, set a location for the folder to be pulled to (destination). The image below shows an example of the folder being pulled into the Document folder in the local drive.

<img src="www/ReadME/2.Choosing%20repo%20location.PNG" width="580"/>

**Step 3. Ensuring the files are up to date**

To ensure that you have the latest version of the Dashboard files, click on "*Fetch origin*" on the GitHub Desktop. This will update your local folder with the most current files in GitHub.

![](www/ReadME/3.Fetch%20origin.PNG)

You can see which files have been updated by looking at the *Date Modified* column in your file explorer. The example below shows that a recent *app* file was updated alongside the *data* folder.

![](www/ReadME/4.File%20explorer%20timestamp.PNG)

You can also see what has been updated in the code on your GitHub Desktop. On the left panel, click on the "Changes" tab. Then click on the R files to see what has been added and removed since the previous version.

![](www/ReadME/5.Highlighted%20codes.PNG)

### Running and navigating the dashboard

**Step 1. Opening the Dashboard**

You will require RStudio to access the dashboard. (Enter instructions to download RStudio. Or an existing BISOE guide?)

Open the "*scenario_dashboard*" folder in your local drive. Click and run the R file named "*app*". Click anywhere on the script, then hold down "ctrl" + "shift" + "enter" on your keyboard to run the script.

This app.R script is the master file used to source the different 'modules' used to generate the dashboard. More details the dependencies are in the "More details" section.

**Step 2. Navigating the dashboard**

The dashboard will open in your default browsers. The homepage provides some additional information on the purpose for each tab. Make sure that the "Data Last Updated" time stamps look correct.

![](C:/Users/gwang/AppData/Local/RStudio/tmp/paste-83180D86.png)

Click on one of the tabs on top (e.g. *Variable Comparison*). Each tab follows a similar structure.

1.  On the left pane, you can select how you want to view the data. For example, in the Variable Comparison tab, you can view graphs and tables by version, by scenario, or by region.
2.  Then click on what you would like displayed. For example, levels versus year-on-year percentage change, and whether you want state level data or national level data. This will affect the drop-down options in the next section.
3.  Now you can choose what data you would like to view. Following the previous example, you can choose a series based on the Scenario, the State, and the Attribute.

Now you'll have a list of Variables that you can display as a chart and a CAGR table. Click on the Variables to toggle on/off the line chart on the graph and column in the CAGR table.

By default, the CAGR tables will have some set periods, but you can add a custom CAGR period in the Variable select section.

![](C:/Users/gwang/OneDrive%20-%20Oxford%20Economics/Pictures/Annotated%20comparion%20page.PNG){width="580"}

### Who do I talk to?

-   Repo owner or admin
-   Other community or team contact

\<\<\<\<\<\<\< Updated upstream

# Testing with README BRANCH

Testing with README BRANCH \>\>\>\>\>\>\> Stashed changes
