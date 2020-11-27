# AssociationRule
Association rules analysis is a technique to uncover how items are associated to each other.
## Apriori Algorithm
  - This algorithm is used for finding frequently occurring itemsets using the boolean association rule.
  - It is called Apriori as it makes use of the ‘prior’ knowledge of the properties in an itemset.
  - In this algorithm an iterative approach is applied. 
  - This is a level-wise search where we mine k-frequently occurring itemset to find k+1 itemsets.
  
- Apriori makes the following assumptions –
  - The subsets of a frequent itemset must also be frequent.
  - Supersets of an in-frequent itemset must also be in-frequent.
  - The three significant components of an Apriori Algorithm are –

    1.Support is a measure of the default popularity (which is a result of frequency) of an item ‘X’.
      It is calculated through the division of the number of transactions in which X appears with the total number of transactions.
     
        S(X) = Number of Transcations in which X Appears \ Total number of transactions 
      
    2.Confidence the division of the total number of transactions involving X and Y with the total number of transactions involving X.
    
        Confidence(X -> Y) = S(X U Y) / S(X) 
      
    3.Lift is the increase in the ratio of the sale of X when you sell the item Y.
    It is used to measure the likelihood of the Y being purchased when X is already purchased, taking into account the popularity of the item Y.
    
        Lift(X -> Y) = S(X U Y) / S(X) * S(Y) 
