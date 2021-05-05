import argparse
import os
import numpy
import vtk
import time
import glob

try:
    import pandas
except:
    print("Error importing pandas package, please install first\n")
    raise

try:
    import matplotlib as mpl
    from matplotlib import cm
    import matplotlib.colors as col
    import matplotlib.pyplot as plt
except:
    print("Error importing matplotlib package, please install first\n")
    raise

try:
    import whitematteranalysis as wma
except:
    print("<wm_register.py> Error importing white matter analysis package\n")
    raise

def main():
    #-----------------
    # Parse arguments
    #-----------------
    parser = argparse.ArgumentParser(
        description="Create a mrml file, which fiber color is based on input value. The mrml file and tract files must in the same folder",
        epilog="Written by Jianzhong He, vsmallerx@gmail.com. ")
    parser.add_argument("-v", "--version",
        action="version", default=argparse.SUPPRESS,
        version='1.0',
        help="Show program's version number and exit")
    parser.add_argument(
        'inputDirectory',
        help='A directory of fiber clusters (.vtk or .vtp).')
    parser.add_argument(
        'inputExcel',
        help='An Excel file with bundles name and color value.')
    parser.add_argument(
        '-norm_off', action="store", dest="color_Normalized_off",
        help='Turn off Normalized color. The minimum value and the maxime value will be -1 and 1 (Default).')
    parser.add_argument(
        '-min', action="store", dest="min_val", type=float, default=-1.,
        help='When turn off Normalized color, set the minimum value.')
    parser.add_argument(
        '-max', action="store", dest="max_val", type=float, default=1.,
        help='When turn off Normalized color, set the maximum value.')
    parser.add_argument(
        '-colormap', action='store', dest="type_of_colormap", type=str, default='seismic',
        help='you can find in https://matplotlib.org/3.1.0/tutorials/colors/colormaps.html, and select each colormap you want.')
    args = parser.parse_args()

    # Load 
    input_excel = args.inputExcel
    input_dir = args.inputDirectory
    mrml_filename = input_excel[:-5] + "_beta_tracts.mrml"

    # Read excel and tract file in folder
    data_frame = pandas.read_excel(input_excel)
    input_polydatas = list()
    for i in range(len(data_frame['Tracts'])):
        name = "{0}/"+data_frame['Tracts'][i]+"*"
        input_mask = name.format(input_dir)
        filepath = glob.glob(input_mask)
        if len(filepath)==0:
            print('Please put the bundle:', data_frame['Tracts'][i], 'into folder:', input_dir)
        else:
            input_polydatas.append(filepath[0])
        
    print("Found", len(input_polydatas), "vtk/vtp files in input directory:", input_dir, "based on excel file:", input_excel)

    # Generate color
    correlations = numpy.array(data_frame['beta'])

    if args.color_Normalized_off:
        correlations[correlations>0] = correlations[correlations>0] * (1/(args.max_val-args.min_val)) - args.min_val/(args.max_val-args.min_val)
        # correlations[correlations<0] = correlations[correlations<0] * -(0.5/args.min_val) + 0.5
        # Normalize = mpl.colors.Normalize(vmin=args.min_val, vmax=args.max_val)
        divnorm = col.DivergingNorm(vmin=args.min_val, vcenter=(args.min_val+args.max_val)/2, vmax=args.max_val)
    else:
        min_val = numpy.min(correlations); max_val = numpy.max(correlations)
        correlations[correlations>0] = correlations[correlations>0] * (1/(max_val-min_val)) - min_val/(max_val-min_val)
        # correlations[correlations<0] = correlations[correlations<0] * -(0.5/min_val) + 0.5
        # Normalize = mpl.colors.Normalize(vmin=min_val, vmax=max_val)
        divnorm = col.DivergingNorm(vmin=min_val, vcenter=(min_val+max_val)/2, vmax=max_val)
        # divnorm = col.DivergingNorm(vmin=min_val, vmax=max_val)

    cmap = cm.get_cmap(args.type_of_colormap)
    colors = cmap(correlations)
    colors = colors * 256

    # Save colorbar
    fig, ax = plt.subplots(figsize=(6, 1))
    fig.subplots_adjust(bottom=0.5)
    cmap = cm.get_cmap(args.type_of_colormap)
    
    cb1 = mpl.colorbar.ColorbarBase(ax, cmap=cmap, norm=divnorm, orientation='horizontal')
    
    cb1.set_label('Colorbar')
    plt.savefig(os.path.join(input_dir, input_excel[:-5] + 'colorbar.png'))

    # Write mrml file
    wma.mrml.write(input_polydatas, colors, os.path.join(input_dir, mrml_filename), ratio=1.0)
    print('Done!')
    
if __name__ == '__main__':
    main()