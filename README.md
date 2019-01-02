# R_Predictive_Maintenance

Predictive Maintenance Project consists of two parts :

**Data Analytics** : Development of machine learning to predict the future failure of routing machines like pumps.

**Data visualization** : web application based on R-shiny as a simple decision-making tool to make easier to use the predictive model.


<h3><i>Sources of Data:</i></h3>

**Sensors_data**:  a **time series** dataset coming from sensors installed on each routing machine. Sensors_data describes the evolution of vibratory health indicators: temperature, and vibrations velocity of  and acceleration. The sensors are connected to a remote server within a platform of **Internet of Things**. The sensors send the data every two hours with at least 200 observations. Time step between two consecutive observations is not regular but can't be more than two minutes. 

**History of failure events**: failures history of the correspondent machine from 2014 to 2017. 

![Screenshot](https://github.com/SoufianeDataFan/R_Predictive_Maintenance_Web_Dashboard/blob/master/Dataflow.png)


<h3><i>Decision making </i></h3>

<li> The question I am trying to answer with this model is: What is the likelihood that a machine is going to fail after a given period of time.
for some industries (e.g mining, manufacturing ...etc), the production line can have some heavy and critical machines. The failure of one of them
will stop the production and cause a huge amount of losses (money or people). 

<li> Sometimes, if one of these machines fails, it takes several months to order a new one.
At the same time, excessive maintenance actions may slow the production and is not cost effective. This predictive model aims to suggest the optimal maintenance strategy that can reduce losses and increase production efficiency.

<li> Practically speaking, let me give you this example : It's Thursday and a maintenance manager (call him Mike) want to have a peaceful weekend. 
Any failure event of a critical machine in the production will force him to go back to the office and fix this problem. 


With this predictive model, Mike will check the failure risk of each machine within ***two days**.
The model will output the failure probability of each one of these machines. Next, he will select the machines with risk probability higher than a specific threshold. In order to have to have a peaceful weekend, Mike should prioritize those machines in a critical state and proceed maintenance to avoid any
in the middle of his weekend :) 


<h3><i>Notes : </i></h3>

<li> The model will make a correlation between the failures history and the 3 vibratory health indicators to predict the future failures events

<li> I decide to focus on survival versions decision trees algorithms like survival random forest.

<li> Time_to_failure is one of the main features that I relied on to estimate the likelihood of failure occurrence 

<li> The input of the model are the machine ID and the number of days, and the model will output the probability of failure of that machine. 


![Screenshot](https://github.com/SoufianeDataFan/R_Predictive_Maintenance_Web_Dashboard/blob/master/Dashboard'.png)


<h3><i>Other application</i></h3>


<li> Healthcare is one of the most interesting domains to apply Survival analysis or time_to_event prediction. It can help to diagnose many types of cancer and other diseases. 
<li> Aerospace is also one of the interesting domains to use this technology. Predicting the failure events of the plane will be very helpful
to prevent many deadly accidents. 




