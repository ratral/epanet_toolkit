# Hydraulic model to understand the reduction of losses in hydraulic systems. 

The idea behind this project is to develop a standard hydraulic model to demonstrate the effects of pressure management and the different models of damage location in water networks.

for this we will use EPANET with R and the packages "epanetReader" and "epanet2toolkit".

## Development of a standard network.

First, we need to create a fictional Hydraulic modelling network sector with measured inputs and for pressure reducing valve controller.

We will define a fictitious _**District Measured Area (DMA)**_ to check the different strategies of pressure management in order to reduce the water losses. DMAs are discrete areas of the _**water distribution system (WDS)_** and are created by closing boundary valves or by permanently disconnecting pipes to other areas so that it remains flexible to changing demands.

Effective undertanding, measurement and calculation of the diferent modells and strategien will help to quantify un-generated water and reduce waste.

**Development of a standard network 100m * 100m** 

## Pressure Management

Calculation of the effect of different pressure management models.

Pressure management can be defined as the practice of managing system pressures to the optimum levels of service while ensuring sufficient and efficient supply to legitimate uses.The positive effects of pressure management are to decrease real water losses by reducing unnecessary or excess pressures.

### One Inlet systems

 - inlet mit constant pressure;
 - inlet with Time-based pressure modulation;
 - inlet with controlled pressure according to the flow __p_t=f(q_t)__;
 - tank effect.



### Multi inlet system



## Comparison of different sensor placement algorithms for model-based leak localization.

The aim of this models is to reduce the area where leakage is occurring in order to make later pinpointing easier. A traditional approach is to divide the network into so-called district metered areas (DMAs).

Leakage monitoring requires the installation of flow meters and pressure sensors at specific points in the DMAs. The inlet flow into and out of the DMAs need to be metered. 


 
