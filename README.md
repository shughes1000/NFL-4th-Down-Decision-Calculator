Using public data from NFL games, I designed a predictive model that suggests whether to punt, kick a field
goal, or go for it on fourth down when given the field position and yards needed for a first down. 

I acquired the play by play data used for this project from Ben Baldwin of the NFLfastR community. Note that 
this dataset contains numerous observations per play, but I felt that this went against the spirit of the 
project I wanted to complete. Therefore, I only used the basic data from the play by play that would be found 
from nearly every play by play source. 

Objectives: 
1) Make a custom expected points model for first downs. 
2) Make a model for field goal conversion percentage with a given field goal distance.
3) Make a model that estimates expected points on punt play when given the yardline.
4) Make a model that estimates conversion percentage on fourth downs given the yardline and yards for a first
down.
5) Make models for yards gained on a fourth down attempt when given the yardline, yards for first down, and 
whether the play converts or not. 

With all of this together, we can make a fourth down calculator. 

Conclusions: Like many NFL analytical projects, my calculator agrees that NFL coaches are typically more risk 
adverse to going for fourth downs than the model recommends. In order for coaches to maximize their expected 
points, they would need to be willing to go for it more frequently according to my model. 

Possible improvements: In many situations, the best decision is not necessarily maximizing points, but instead 
maximizing the probability that the team wins the game (win probability). The reason that I did not do this 
was because I wanted a more theoretical analysis that suggested what to do in neutral situations. However, 
using the conversion percentages acquired by this project and the win probability model in the play by play 
data, we could make a 4th down model that uses win probability instead of expected points. 
