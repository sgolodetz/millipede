/***
 * millipede: SegmentationRun.h
 * Jess Pumprey, 2012
 ***/

#ifndef H_MILLIPEDE_SEGMENTATIONRUN
#define H_MILLIPEDE_SEGMENTATIONRUN

#include <sstream>

#include <common/jobs/Job.h>

#include <common/math/NumericUtil.h>
#include <mast/models/PartitionModel.h>
#include <mast/util/StringConversion.h>
#include <common/segmentation/DICOMLowestLayersBuilder.h>
#include <common/segmentation/VolumeIPFBuilder.h>
#include <iostream>

namespace mp { 

template <typename LeafLayer, typename BranchLayer, typename Feature>

/*****
 * Represents a segmentation job, to be combined with others to compare their results.
 * 
 * 
 * 
 * 
 * 
 * 
 * ****/


class SegmentationRun {
public:
	
	typedef VolumeIPF<LeafLayer,BranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef VolumeIPFBuilder<DICOMLowestLayersBuilder> DICOMVolumeIPFBuilder;

	SegmentationRun(const DICOMSegmentationOptions * options) {
		m_options = options;
		m_volumeIPF = boost::shared_ptr<VolumeIPFT>();
	}
	
	Job_Ptr getJob() {
		return m_job;
	}
	
	VolumeIPF_Ptr getVolumeIPF() {
		//std::cout << "sr::getipf: m_volumeIPF = " << m_volumeIPF << std::endl;
		//std::cout << "sr::getipf: &m_volumeIPF = " << &m_volumeIPF << std::endl;
		return m_volumeIPF;
	}
	
	void make_job(const DICOMVolume_CPtr& volume) {
		//m_volumeIPF = ipf;
		//std::cout << "sr::makejob: m_volumeIPF = " << m_volumeIPF << std::endl;
		//std::cout << "sr::makejob: &m_volumeIPF = " << &m_volumeIPF << std::endl;
		m_job.reset(new DICOMVolumeIPFBuilder(volume,*m_options,m_volumeIPF));
	}
	
	const DICOMSegmentationOptions* getOptions() {
		return m_options;
	}
	
	wxColour getColour() {
		return m_colour;
	}
	
	void setColour(wxColour c) {
		m_colour = c;
	}
	
	/*void setTexture(Greyscale8SliceTextureSet_Ptr t) {
		texture = t;
	}
	
	Greyscale8SliceTextureSet_Ptr getTexture() {
		return texture;
	}*/
	
private:
	
	//options for this segmentation
	const DICOMSegmentationOptions * m_options;
  
	//colour to draw the result
	wxColour m_colour;
  
	//process of running the segmentation wrapped as a job
	Job_Ptr m_job;
	
	//resulting ipf - job execution should write to this.
	VolumeIPF_Ptr m_volumeIPF;
	
	//cached texture
	//Greyscale8SliceTextureSet_Ptr texture;
	
	
};

}
  


#endif