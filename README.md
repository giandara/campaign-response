# Predicting the response rate of a marketing campaign, using a dataset from ABC insurance company
## Project context
The ABC insurance company is planning a new marketing campaign for their insurance policy product. It is costly for the company
to advertise to customers that seem likely to renew (response = Yes), so the company wants to create a tool to predict how customers of 
various customer segments will response to the campaign. The insurance company provided a historical dataset of their existing customers,
and the goal of the model is to predict which customers will respond (respond = Yes) and minimize false positive margin.

## Csv file data dictionary
- Customer: unique customer identification number
- State: state location of customer
- Customer Lifetime Value: customer lifetime value
- Coverage: type of auto insurance coverage (Basic, Extended, Premium)
- Education: education level of the customer
- Effective To Date: date until which the current policy is effective
- EmploymentStatus: the customer’s employment status
- Gender: the customer’s gender
- Income: the customer’s yearly income
- Location Code: the description of the customer’s location
- Marital Status: the marital status of the customer
- Monthly Premium Auto: the monthly premium amount for the customer’s automotive insurance policy
- Months Since Last Claim: the number of months since the customer last submitted an insurance claim
- Number of Policies: the number of policies that the customer has with the company
- Policy Type: the type of policy that the customer has with the company
- Policy: the specific policy that the customer has with the company
- Renew Offer Type: the renewal offer extended to the customer during the last campaign
- Sales Channel: the medium in which the renewal offer was extended to the customer
- Total Claim Amount: the total amount of submitted claims for the customer
- Vehicle Class: the class type of the customer’s vehicle
- Vehicle Size: the size type of the customer’s vehicle
- Response: the customer’s response to the last marketing campaign

# Methodology
- The clustering analysis helps to identify segments of the customers, helping the insurance company to create target offers and localize the campaign
to the groups that will be more likely to response.
- The decision tree and naive bayes methods are utilized to predict the campaign response rate. Hyperparameters are tuned to improve the model accuracy.
