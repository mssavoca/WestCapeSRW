#!/usr/bin/env python
from __future__ import division
import scipy.io as sio
from numpy import *
import os
from os import system
import csv 
import getopt
import sys
from pylab import *
import scipy.signal as signal
from scipy import interpolate
import pandas as pd
from scipy.interpolate import splev, splrep
from mpl_toolkits.mplot3d.art3d import Line3DCollection
from mpl_toolkits.mplot3d import Axes3D

#import matlab.engine

#############################################################################################################################################
def plot_traj(pitch_smoothed2, roll_smoothed2,heading_smoothed2,speed_smoothed2, depth_smoothed2,fs,title_name,fig_path,starts,ends):

        X= zeros_like(heading_smoothed2)
        Y= zeros_like(heading_smoothed2)
        Z= zeros_like(heading_smoothed2)

        Fx= zeros_like(heading_smoothed2)
        Fy= zeros_like(heading_smoothed2)
        Fz= zeros_like(heading_smoothed2)

        Dx= zeros_like(heading_smoothed2)
        Dy= zeros_like(heading_smoothed2)
        Dz= zeros_like(heading_smoothed2)
    
        for n in range(0,len(heading_smoothed2)-1):

            Fx[n]=speed_smoothed2[n]*(1.0/fs)*cos(radians(-heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            Fy[n]=speed_smoothed2[n]*(1.0/fs)*sin(radians(-heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            Fz[n]=speed_smoothed2[n]*(1.0/fs)*sin(radians(pitch_smoothed2[n]))

            X[n]=sum(Fx[:n])
            Y[n]=sum(Fy[:n])
            Z[n]=-depth_smoothed2[n] #sum(Fz[:n])-depth_smoothed2[0]

            F=[Fx[n],Fy[n],Fz[n]]/sqrt((Fx[n]**2)+(Fy[n]**2)+(Fz[n]**2))

            ###
            vert=[0,0,1]
            right=cross(F,vert[:])
            dorsal=cross(right[:],F)
    
            dorsal_roll=dorsal*cos(radians(roll_smoothed2[n]))+cross(F,dorsal[:])*sin(radians(roll_smoothed2[n]))
    
            Dx[n]=dorsal_roll[0]
            Dy[n]=dorsal_roll[1]
            Dz[n]=dorsal_roll[2]


    ###plot
        o=int(fs*1) # Changing to 1 from 10 so now will have sample at full sampling rate of 10

        clf()
        fig1=figure(figsize=(12,6))
        subplot(121)
        #color=speed_smoothed2[::o]/amax(speed_smoothed2[:])        
        for n in range(0, len(heading_smoothed2),o):        
            x = (3)*cos(radians(heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            y = (3)*sin(radians(heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            z = (3)*sin(radians(pitch_smoothed2[n]))
            plot([X[n]-x,X[n]],[Y[n]+y,Y[n]],c='r',lw=1,zorder=1)

            #scatter(X[n],Y[n],c=roll_smoothed2[n],s=50,lw=.5,edgecolors='k',cmap=cm.RdBu,vmin=-180,vmax=180,zorder=2) #color by roll
            scatter(X[n],Y[n],c=speed_smoothed2[n],s=50,lw=0.1,cmap=cm.cool,vmin=0,vmax=5,zorder=2) #color by speed

        scatter(X[starts],Y[starts],c='g',s=50,lw=0.1,zorder=3)
        scatter(X[ends],Y[ends],c='r',s=50,lw=0.1,zorder=3)
           
        #for n in range(0, len(starts)):
            #scatter(X[starts[n]],Y[starts[n]],c='g',s=50,lw=0.1,zorder=3)
            #scatter(X[ends[n]],Y[ends[n]],c='r',s=50,lw=0.1,zorder=3)

        #set axis to square, then get xlims with xlim() and round to nearest 10, then set xlim and ylim 
        axis('square')
        a,b=round(xlim()[0],-1),round(xlim()[1],-1)
        xlim(a,b)
        ylim(round(max(Y),-1)-(abs(a)+abs(b)), round(max(Y),-1))
        title('top: '+str(title_name))

        subplot(122)
        for n in range(0, len(heading_smoothed2),o):
            x = (3)*cos(radians(heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            y = (3)*sin(radians(heading_smoothed2[n]))*cos(radians(pitch_smoothed2[n]))
            z = (3)*sin(radians(pitch_smoothed2[n]))
            plot([X[n]-x,X[n]],[Z[n]-z,Z[n]],c='r',lw=1,zorder=1)

            #scatter(X[n],Z[n],c=roll_smoothed2[n],s=50,lw=0.5,edgecolors='k',cmap=cm.RdBu,vmin=-180,vmax=180,zorder=2) #color by roll
            scatter(X[n],Z[n],c=speed_smoothed2[n],s=50,lw=0.1,cmap=cm.cool,vmin=0,vmax=5,zorder=2) #color by speed   

        scatter(X[starts],Y[starts],c='g',s=50,lw=0.1,zorder=3)
        scatter(X[ends],Y[ends],c='r',s=50,lw=0.1,zorder=3)

        #for n in range(0, len(starts)):
            #scatter(X[starts[n]],Z[starts[n]],c='g',s=50,lw=0.1,zorder=3)

        #hlines(0,a,b)        
        axis('square')
        xlim(a,b)
        ylim(-(abs(a)+abs(b)),0)
        title('side: '+str(title_name))

        savefig(fig_path)
        fig1.clear()
        close(fig1)
        #show()
        #################################################

        if 1:
            from mpl_toolkits.mplot3d import Axes3D

            clf()
            fig = plt.figure(1)
            ax = fig.add_subplot(111,projection='3d')

            # Change the background color of the figure and the axes
            fig.patch.set_facecolor('white')  # Set the figure background color to white
            ax.set_facecolor('white')  # Set the axes background color to white

            #for n in range(0, len(heading_smoothed2),o):
                #scatter_plot = ax.scatter(X[n],Y[n],Z[n],lw=0,s=20, c=[speed_smoothed2[n]],cmap = cm.plasma,vmin=0.4,vmax=1,zorder=2)#color by speed
        
                #ax.scatter(X[n],Y[n],Z[n],lw=.5,edgecolors='k',s=20, c=roll_smoothed2[n],cmap=cm.RdBu,vmin=-180,vmax=180,zorder=2)#color by roll

            # Create an array of points from the X, Y, Z coordinates
            points = np.array([X, Y, -Z]).T  # Stack X, Y, Z into a 2D array (each row is a point)

            # Create segments for LineCollection (between consecutive points)
            segments = [points[i:i+2] for i in range(len(points)-2)]

            # Create a colormap based on the third variable (C)
            norm = plt.Normalize(vmin= 0.4, vmax=1.8)  # Normalize to range of C
            cmap = plt.get_cmap('plasma')  # You can change the colormap to your preference

            # Create a LineCollection for the line segments
            lc = Line3DCollection(segments, cmap=cmap, norm=norm, linewidth=2)
            lc.set_array(speed_smoothed2[:-1])  # Assign the third variable (C) to the segments

            # Add the LineCollection to the plot
            ax.add_collection(lc)

            # Create a plane at z = 0
            x_plane = np.linspace(min(X)-10, max(X)+10, 10)  # x values covering the range of the data, adding 10 to make nice plot
            y_plane = np.linspace(min(Y)-10, max(Y)+10, 10)  # y values covering the range of the data, adding 10 to make nice plot
            X_plane, Y_plane = np.meshgrid(x_plane, y_plane)  # Create a meshgrid for X and Y
            Z_plane = np.zeros_like(X_plane)  # Set Z values to 0 (the plane)

            # Plot the plane at z = 0
            ax.plot_surface(X_plane, Y_plane, Z_plane, color='lightgray', alpha=0.25, linewidth=0)

            # Get the current x and y ticks
            x_ticks = ax.get_xticks()
            y_ticks = ax.get_yticks()

            # Create new x-tick labels as relative values
            x_ticks_new = x_ticks - x_ticks[0]  # Shift all ticks to make the first one 0
            y_ticks_new = y_ticks - y_ticks[0]  # Shift all ticks to make the first one 0

            # Set new x-tick labels
            ax.set_xticks(x_ticks)  # Keep the original tick positions
            ax.set_xticklabels([f"{t:.0f}" for t in x_ticks_new])  # Update labels to be relative
            ax.set_yticks(y_ticks)  # Keep the original tick positions
            ax.set_yticklabels([f"{t:.0f}" for t in y_ticks_new])  # Update labels to be relative

            # Set custom tick labels to show only the first and last labels
            #ax.set_xticklabels([str(abs(x_ticks[0])), str(x_ticks[-1])])
            #ax.set_yticklabels([str(y_ticks[0]), str(y_ticks[-1])])

            # Plotting the axes labels
            #ax.set_xlabel('X Axis')
            #ax.set_ylabel('Y Axis')
            ax.set_zlabel('Depth (m)')

            # Invert the Z-axis
            ax.invert_zaxis()     

            ax.scatter(X[starts],Y[starts],-Z[starts],c='g',s=50,lw=0.1,zorder=3)
            ax.scatter(X[ends],Y[ends],-Z[ends],c='r',s=50,lw=0.1,zorder=3)

            # for n in range(0, len(starts)):
            #     ax.scatter(X[starts[n]],Y[starts[n]],-Z[starts[n]],c='g',s=50,lw=0.1,zorder=3)
            #     ax.scatter(X[ends[n]],Y[ends[n]],-Z[ends[n]],c='r',s=50,lw=0.1,zorder=3)

            ax.set_xlim(min(X), max(X))
            ax.set_ylim(min(Y), max(Y))
            ax.set_zlim(0, 80)

            # Reduce gridlines (optional: control grid appearance)
            ax.grid(True)  # Enable grid
            ax.grid(linewidth=0.5, linestyle='-', color='gray')  # Lighter and thinner gridlines

            # Customize the gridline appearance (optional)
            ax.xaxis.pane.fill = False  # Hide the X-axis pane
            ax.yaxis.pane.fill = False  # Hide the Y-axis pane
            ax.zaxis.pane.fill = False  # Hide the Z-axis pane
       
            #ax.axis('equal')

            # Add a colorbar to show the mapping of color to the third variable (C)
            plt.colorbar(lc, ax=ax, label='Speed (m/s)')



#            savefig(path+'/3d_view.svg',transparent='True')
            plt.show()


        ##########measure distance traveled
        if 1:
            #total
            vec=empty((len(X)-2))
            for n in range(0, len(vec)):
                vec[n]=sqrt(((X[n+1]-X[n])**2)+((Y[n+1]-Y[n])**2)+((Z[n+1]-Z[n])**2))
            distance_total=sum(vec)
            #print('distance_total', distance_total)

            #feeding
            distance_feeding=sum(vec[starts:ends])
            #print('distance_feeding', distance_feeding)

            #xyz distance covered during feeding
            straight_line_distance=sqrt(((X[ends]-X[starts])**2)+((Y[ends]-Y[starts])**2)+((Z[ends]-Z[starts])**2))
            #print('straight_line_distance',straight_line_distance)    
        
            tortuosity=1-(straight_line_distance/distance_feeding)
            #print('tortuosity', tortuosity)

            return(distance_total,distance_feeding,straight_line_distance,tortuosity)

########################################################

def main(infile, infile2):

    dive_file=pd.read_csv(infile2)

    if 1:
        x = sio.loadmat(infile)
        folder='new_figures'
        
        tagon=x['tagon']
        frameO=arange(0,len(tagon))
        startframe=frameO[where(tagon==1)[0][0]]
        stopframe=frameO[where(tagon==1)[0][-1]]
        #stopframe=200000

        startframe_index=where(frameO==int(startframe))[0][0]
        stopframe_index=where(frameO==int(stopframe))[0][0]

        depth=x['p'][startframe_index:stopframe_index].reshape(-1,)

        speed=x['speed_JJ'][startframe_index:stopframe_index].reshape(-1,)

        ######
        #eng = matlab.engine.start_matlab()
        #input = "s = load('%s');" % infile
        #eng.evalc(input)
        #speed=np.array(eng.eval("s.speed.JJ"))[startframe_index:stopframe_index].reshape(-1,) #changed asarray to np.array to solve read-only array problem for speed
        ######

        Ax=x['Aw'][:,0][startframe_index:stopframe_index]
        Ay=x['Aw'][:,1][startframe_index:stopframe_index]
        Az=x['Aw'][:,2][startframe_index:stopframe_index]
    
        Mx=x['Mw'][:,0][startframe_index:stopframe_index]
        My=x['Mw'][:,1][startframe_index:stopframe_index]
        Mz=x['Mw'][:,2][startframe_index:stopframe_index]

        Gx=x['Gw'][:,0][startframe_index:stopframe_index]
        Gy=x['Gw'][:,1][startframe_index:stopframe_index]
        Gz=x['Gw'][:,2][startframe_index:stopframe_index]
    
        frame=frameO[startframe_index:stopframe_index]
        
        DN=x['DN'][:,0][startframe_index:stopframe_index]
    
        fs=int(x['fs'][0][0])

        ##########################################################################
        #filterdesign. CF1=0.4. This does not separate stroke from body position.

        bOrder=2    
    
        if 1:
            cutoff_frequency1=float(0.4)  #hz
            cutoff_frequency2=float(0.4)  #hz  
 
        Wn=cutoff_frequency1/(fs/2)
        Wn2=cutoff_frequency2/(fs/2)

        #################################################################################

        #filter with lowpass Wn to separate body from stroke
        
        b,a = signal.butter(bOrder,Wn,btype='low')
        Ax_Filt = signal.filtfilt(b,a,Ax[:])[:]
        Ay_Filt = signal.filtfilt(b,a,Ay[:])[:]
        Az_Filt = signal.filtfilt(b,a,Az[:])[:]

        ###Here, we filter A and M. For M, must fill in blank mag signal during camera off and on, for smoothing. Later, for turns, discard sequences with any NoMag=True.
        NoMag=isnan(Mx)

        for N in range(0, len(Mx)):
            if isnan(Mx)[N]==True:
                Mx[N]=Mx[N-1]
                My[N]=My[N-1]
                Mz[N]=Mz[N-1]  

        Mx_Filt = signal.filtfilt(b,a,Mx[:])[:]
        My_Filt = signal.filtfilt(b,a,My[:])[:]
        Mz_Filt = signal.filtfilt(b,a,Mz[:])[:]

        #filter gyro with Wn2 to remove sampling noise. Not Wn because we don't want to remove strokes.   
        b,a = signal.butter(bOrder,Wn2,btype='low')
        Gx_Filt = signal.filtfilt(b,a,Gx[:])[:]
        Gy_Filt = signal.filtfilt(b,a,Gy[:])[:]
        Gz_Filt = signal.filtfilt(b,a,Gz[:])[:]

        #calculate filtered pitch and roll:
        Am=sqrt((Ax_Filt*Ax_Filt)+(Ay_Filt*Ay_Filt)+(Az_Filt*Az_Filt))
        pitch_smoothed=(arcsin(Ax_Filt/Am))    
        roll_smoothed=((arctan2(Ay_Filt, Az_Filt)))-pi

        heading_smoothed=zeros_like(roll_smoothed)
        M=vstack((Mx_Filt,My_Filt,Mz_Filt)).T
        Mh= zeros_like(M)

        for n in range(0,len(Mx_Filt)):
            YRotationAngle=pitch_smoothed[n]
            XRotationAngle=roll_smoothed[n]

            RotY = array(((cos(YRotationAngle),0,sin(YRotationAngle)),(0,1,0),(-sin(YRotationAngle),0,cos(YRotationAngle))))
            RotX = ((1,0,0),(0,cos(XRotationAngle),-sin(XRotationAngle)),(0,sin(XRotationAngle),cos(XRotationAngle)))
        
            M_tmp=M[n,:]
            M_tmp=dot(RotX,transpose(M_tmp))
            M_tmp=dot(RotY,transpose(M_tmp))
            Mh[n,:]=M_tmp

            heading_smoothed[n]=-arctan2(Mh[n,1], Mh[n,0])

        #filter speed and depth (at wn)
        speed[isnan(speed)]=0

        b2,a2 = signal.butter(bOrder,Wn,btype='low')
        speed_smoothed=signal.filtfilt(b2,a2,speed.reshape(-1,))[:]

        depth_smoothed=signal.filtfilt(b2,a2,depth)[:]

        #############
        pitch_smoothed=degrees(pitch_smoothed)
        heading_smoothed=degrees(unwrap(heading_smoothed))    
        roll_smoothed=degrees(unwrap(roll_smoothed))

        heading_smoothed_wrapped = heading_smoothed % 360
        heading_smoothed_wrapped[where(heading_smoothed_wrapped>180)]=heading_smoothed_wrapped[where(heading_smoothed_wrapped>180)]-360

        roll_smoothed_wrapped = roll_smoothed % 360
        roll_smoothed_wrapped[where(roll_smoothed_wrapped>180)]=roll_smoothed_wrapped[where(roll_smoothed_wrapped>180)]-360

        ##########################################################################
        #fluke stroke: based on Gough's papers
        pitch_stroke=Gy_Filt   

        ###########################################################################
        #jerk
        jerk=sqrt((diff(Ax)**2)+(diff(Ay)**2)+(diff(Az)**2))*fs
        jerk[where(isnan(jerk)==True)]=0

        ###########################################################################
        #hack: add array of zeros to start to get to matlab frame numbers
        if 1:
            speed_smoothed = hstack([zeros(startframe+1),speed_smoothed])
            depth = hstack([zeros(startframe+1),depth])
            depth_smoothed = hstack([zeros(startframe+1),depth_smoothed])
            pitch_smoothed = hstack([zeros(startframe+1),pitch_smoothed])
            roll_smoothed = hstack([zeros(startframe+1),roll_smoothed])
            heading_smoothed = hstack([zeros(startframe+1),heading_smoothed])
            roll_smoothed_wrapped = hstack([zeros(startframe+1),roll_smoothed_wrapped])
            heading_smoothed_wrapped = hstack([zeros(startframe+1),heading_smoothed_wrapped])
            pitch_stroke = hstack([zeros(startframe+1),pitch_stroke])
            jerk=hstack([zeros(startframe+1),jerk])    

        ###########################################################################
        #run calculations        
        start_array=array(dive_file.dive_start)
        stop_array=array(dive_file.dive_end)

        feeding_start=array(dive_file.feeding_start)
        feeding_end=array(dive_file.feeding_end)
        
        for i in range(0,len(start_array)):
                start_fig=int(start_array[i])
                stop_fig=int(stop_array[i])
                print(start_fig,stop_fig)

                figure_title=start_array[i]
                (path, file) = os.path.split(infile2)
                figure_path=('C:\\Users\\ashle\\Dropbox\\Ashley\\Post-Doc\\Projects\\SRW diving\\3d_figures\\%i.png' %start_array[i])      
                distance_total,distance_feeding,straight_line_distance,tortuosity=plot_traj(pitch_smoothed[start_fig:stop_fig],roll_smoothed_wrapped[start_fig:stop_fig],heading_smoothed[start_fig:stop_fig]-heading_smoothed[start_fig],speed_smoothed[start_fig:stop_fig],depth_smoothed[start_fig:stop_fig],fs,figure_title,figure_path,int(feeding_start[i]-start_fig),int(feeding_end[i]-start_fig))

                dive_file.loc[i, "dist_total"] = distance_total
                dive_file.loc[i, "dist_feeding"] = distance_feeding
                dive_file.loc[i, "staightline_dist_feeding"] = straight_line_distance
                dive_file.loc[i, "tortuosity_feeding"] = tortuosity

        ###### other calculations:
        for i in range(0,len(start_array)):
                pitch_smoothed[feeding_start[i]:feeding_end[i]]
                roll_smoothed_wrapped[feeding_start[i]:feeding_end[i]]
                heading_smoothed[feeding_start[i]:feeding_end[i]]
                speed_smoothed[feeding_start[i]:feeding_end[i]]

                dive_file.loc[i, "avg_pitch_feeding"] = average(pitch_smoothed[feeding_start[i]:feeding_end[i]])
                dive_file.loc[i, "max_roll_feeding"] = max(abs(roll_smoothed_wrapped[feeding_start[i]:feeding_end[i]]))

                #check for pitches above abs(60deg)
                if amax(pitch_smoothed[feeding_start[i]:feeding_end[i]])>60:
                    dive_file.loc[i, "max_roll_feeding"] =nan
               
                if amin(pitch_smoothed[feeding_start[i]:feeding_end[i]])<-60:
                    dive_file.loc[i, "max_roll_feeding"] =nan

        for i in range(1,len(start_array)):
                dive_file.loc[i-1, "time_between_dives"] = ((dive_file.dive_start[i]-dive_file.dive_end[i-1])/fs)        


        dive_file.to_csv(infile2[:-4]+'_v2.csv',index=False, na_rep='nan')

#####################################################################################################################################################
if __name__ == '__main__':

       try:
           opts, args = getopt.getopt(sys.argv[1:], 'h', ['help'])
  

       except getopt.error:
           sys.stderr.write("%s: %s \nTry `%s --help` for more informatIon\n"
               % (PROGNAME, e, PROGNAME))
           sys.exit(1)


       for o in opts:
           if o in ('-h', '--help'):
               usage(error=0)

       if len(args) != 2:
           usage()

       main(args[0],args[1])
