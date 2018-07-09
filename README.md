# Hydraulic model to understand the reduction of Real Water Losses in Water Distribution System (WDS). 

The idea behind this project is to develop a standard hydraulic model to demonstrate the effects of pressure management and the different models of damage location in water networks. For this we will use EPANET with R and the packages "epanetReader" and "epanet2toolkit".

First, we need to create a fictional Hydraulic modelling network sector with measured inputs and for pressure reducing valve ontroller.

Second, We will define a fictitious _**District Measured Area (DMA)**_ to check the different strategies of pressure management in order to reduce the water losses. DMAs are discrete areas of the _**water distribution system (WDS)**_ and are created by closing boundary valves or by permanently disconnecting pipes to other areas so that it remains flexible to changing demands.

An effective undertanding, measurement and calculation of the diferent modells and strategien will help to quantify un-generated water and reduce waste.

Tasks:

 - [x]  Development of a standard distribution network (PRV_01.inp)
 - [x]  Water Demand Patterns:
     - [x]  Different periods of the year (Spring, Summer, Fall, Winter and Summerbreak);
     - [x]  Two different types of day for each time period exist (working-days and holidays-weekends). 
 - [ ]  Report (report_prv_01.Rmd) 


## The effect of pressure on leakage Water Distribution System (WDS).

One of the major factors influencing leakage is the pressure in the distribution system. In the past the conventional view was that leakage from water distribution systems is relatively insensitive to pressure, as described by the orifice equation:

<img src="https://latex.codecogs.com/svg.latex?\Large&space;x=C_d*A*\sqrt({2gh})" title="\Large x=C_d*A*\sqrt({2gh})" />


q = D * A * SQR(2*g*h)

where q the flowrate, C the discharge coefficient, A the orifice area, g acceleration due to gravity and h the pressure head. To apply this equation to leaks in pipes it can be written in more general form as: 
 
![\Large x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}](https://latex.codecogs.com/svg.latex?x%3D%5Cfrac%7B-b%5Cpm%5Csqrt%7Bb%5E2-4ac%7D%7D%7B2a%7D)

q = c*(p^α)

where c is the leakage coefficient and α the leakage exponent. A number of field studies have shown that α can be considerably larger than 0.5, and typically varies between 0.5 and 2.79 with a median of 1.15.


## Indicators for Real Losses and non-revenue water 

 - **(litres) / (conetion) / (day) / (m pressure)**
 - **(litres) / (km pipe) / (day) /  (m pressure)**
 - **ILI** Ratio of Current Annual Real Losses to Unavoidable Annual Real Losses, most powerful indicator for comparisons between systems.
 
## Pressure Management

Calculation of the effect of different pressure management models.

Pressure management can be defined as the practice of managing system pressures to the optimum levels of service while ensuring sufficient and efficient supply to legitimate uses.The positive effects of pressure management are to decrease real water losses by reducing unnecessary or excess pressures.

Tasks for One Inlet in DMAs systems

 - [ ]  inlet mit constant pressure;
 - [ ]  inlet with Time-based pressure modulation;
 - [ ]  inlet with controlled pressure according to the flow;
 - [ ]  tank effect.

Tasks for Multi inlet in DMAs systems

- [ ]  Teil Tank : Water inlet from a tank in head and tail locations of the DMA

## Comparison of different sensor placement algorithms for model-based leak pre-localization.

The term "Leakage awareness methods" ist used to explain the discovery of a leak in a particular area within the network. It does not give any information about its precise location. 

For the leak awareness method is a hidraulic model needed. Different type hydraulic models have been proposed to detect leaks in WDS. Those methods usually involve calibration/optimisation techniques to analyse the different areas of the network. 

The calibration/optimisation requires the installation of flow meters and pressure sensors at specific points in the DMAs. The inlet flow into and out of the DMAs need to be metered. 

The aim of this hidraulic models is to reduce the area where leakage is occurring in order to make later pinpointing easier. A traditional approach is to divide the network into DMAs.





 
