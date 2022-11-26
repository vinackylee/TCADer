# TCADer

This project is based on chipyard 1.2:  https://github.com/ucb-bar/chipyard

So first you should build chipyard 1.2 as per this documentation site: https://chipyard.readthedocs.io/

 

### Directory Structure

-- acc                      --> *accelerator source code*

-- generators        --> *Modified boom, rocket, chipyard*

-- sim-test             --> *Sample Bare Metal Testing Procedure*

-- tcader			    --> **The main code of TCADer，CoMU，LMMU，SDMA，Cache**





#### Build BOOM+ACC with TCADer

Replace the original chipyard code with the modified `generators`, and then put `tcader` and `acc` in the `generators/boom/src/main/scala/` directory.

The config with TCADer framework and accelerator has been written in BoomConfig.scala, just compile it in the original way of chipyard.



#### Test

See run.sh for test program compilation.
