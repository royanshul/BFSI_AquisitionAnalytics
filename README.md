# BFSI_AquisitionAnalytics
Requirement and Implementation steps are below:

The Requirement is to predict the probability of response from each prospect and target the ones most likely to respond to the next telemarketing campaign. The steps were as follows:
Through EDA, identify the relevant predictor variables for response
Build predictive models and choose the best one
Sort the prospects in decreasing probability of response (predicted by the best model) and decide to target the top X% (or top Y deciles), where X would be determined by your business objective (e.g. maximising the overall response rate/the number of responders at a fixed marketing cost)
 
When you look at the important variables included in the final model, you will see a variable ‘duration’ — the duration of the phone call in seconds. When you present this model to the Chief Marketing Officer (CMO), she would note that ‘duration’ has a positive correlation with the response, which is a problem for the marketing team. As the duration increases, the cost of telemarketing increases linearly, and that is the last thing they want.
 
 
There are two problems with having the variable ‘duration’ in the model:
When the marketing team procures prospect data, ‘duration’ is not present in it, since the call hasn’t been made yet
In your analysis of marketing cost and response, you had assumed that the cost of a phone call is independent of duration (₹1 per call) — which is not true
 
Tasks
To solve these problems, you and the CMO should decide to build another model without the variable ‘duration’. That will help you understand the relationship of other variables with the response.
 
You also decide that the business objective is to achieve 80% of total responders at the minimum possible cost. The total number of responders is the total number of prospects who responded, from the available data of about 45,000 data points.
 
To find the number of prospects you should target (i.e. how many top deciles you should target), you will assume that the cost of a call varies with duration as follows:
 
Cost per call (INR) = 0.033*(duration_in_seconds) + 0.8
 
Based on this, you will figure out the X in top X%, i.e. how many prospects should be called to meet the business objective.
 
