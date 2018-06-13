(*squishes the density matrix along the x direction. This allows us \
to see howo the initial beam spreads breaks up as the simulation \
progresses. Output is a dataset of mean density along a column vs \
position on the y axis *)
squish[file_] := Module[{rho, dim},
  rho = readdensity[file];
  dim = Max[Dimensions[rho]];
  (*hard coded the dimensionality of array. 
  need to change if I change other parameters*)
  
  Transpose[{Table[-dim/1024 + (dim/512) j/(dim - 1), {j, 0, 
      dim - 1}], Mean[Transpose[readdensity[file]]]}]
  ]




  (*calculates the spread of the beam based on the standard deviation \
of the squished density profile. We set a normalization such that \
spred = 1 for the IC*)

spread[file_] := Module[{yvals, xvals, dxvals, mean, out},
  xvals = squish[file][[All, 1]];
  yvals = squish[file][[All, 2]];
  (*chop low density areas to remove hot gas*)
  
  yvals = Map[If[# > 1.5, #, 0.0] &, yvals]; 
  (*normalize such that area under curve is 1. We set \[CapitalSigma] \
yvals (dxvals) = 1*)
  
  dxvals = (Last[xvals] - First[xvals])/Length[xvals];
  yvals = yvals/(Total[yvals]*dxvals);
  (*yvals is now a physical probability distribution of what y value \
a given mass is likely to be at. integral of ydx = 1.0*)
  
  mean = Total[yvals*xvals*dxvals];
  out = Sqrt[Total[yvals*(xvals - mean)^2*dxvals]];
  Transpose[{xvals, yvals}];
  out/0.07503255862048465(*normalize so spread is 1 initially*)
  ]


  (*function to plot squished data*)

myplot[data_] := 
 ListPlot[data, Joined -> True, 
  AxesLabel -> {Style["y coord", 15], 
    Style["<\[Rho]\!\(\*SubscriptBox[\(>\), \(sightline\)]\)", 15]}, 
  PlotRange -> Full, PlotLegends -> {"Initial", "Final"}]


     exportpic[num_, mach_] := Module[{d1, d2, d3, d4, pic},
  pic = myplot[
    squish["/Volumes/LaCie/simdata/khcool/initially_at_floor/big_box_\
1e5/" <> mach <> "/merged/khcool." <> num <> ".vtk"]];
  Export["/Users/NeerajAir/Desktop/columation/" <> mach <> "/" <> 
    num <> ".png", pic]
  ]
