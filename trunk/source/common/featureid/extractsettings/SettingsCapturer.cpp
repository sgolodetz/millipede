/***
 * millipede: SettingsCapturer.cpp
 * Jess Pumphrey, 2012
 ***/

#include "SettingsCapturer.h"

#include <boost/bind.hpp>
#include <iostream>

namespace mp {

//#################### CONSTRUCTORS ####################
SettingsCapturer::SettingsCapturer(const DICOMVolume_CPtr& dicomVolume, const VolumeIPF_Ptr& volumeIPF, FIOptions * map, VolumeIPFMultiFeatureSelection_Ptr mfs)
{
	
	m_dicomVolume = dicomVolume;
	m_volumeIPF = volumeIPF;
	m_map = map;
	m_mfs = mfs;

}

//#################### PUBLIC METHODS ####################



void SettingsCapturer::execute_impl() {
	
	std::cout << "capturing settings" << std::endl;
	std::cout << "Aorta" << std::endl;
	get_feature_settings(AbdominalFeature::AORTA, std::string("A"), 4);
	
	
	std::cout << "Kidney L" << std::endl;
	get_grow_settings(AbdominalFeature::KIDNEY_LEFT, std::string("K"), 4, SettingsCapturer::LEFT);
	
	
	std::cout << "Kidney R" << std::endl;
	get_grow_settings(AbdominalFeature::KIDNEY_RIGHT, std::string("K"), 4, SettingsCapturer::RIGHT);
	
	std::cout << "Liver" << std::endl;
	get_grow_settings(AbdominalFeature::LIVER, std::string("L"), 3, SettingsCapturer::LEFT);
	
}

/**
 * Finds maximum and minimum Hounsfield values and sizes for organs which do not use region growing.
 */
void SettingsCapturer::get_feature_settings(AbdominalFeature::Enum feature, string organPrefix, int layer) {
	
	PartitionForestSelection_CPtr selection = m_mfs->selection(feature);
	
	int maxH = -10000;
	int minH = 10000;
	int minVox = 100000;
	int maxVox = -1;
	
	for (PartitionForestSelectionT::ViewNodeConstIterator it = selection->view_at_layer_cbegin(layer); it != selection->view_at_layer_cend(layer); ++it) {
		
		BranchProperties properties = m_volumeIPF->branch_properties(*it);
		
		if (properties.mean_houndsfield_value() < minH) {minH = properties.mean_houndsfield_value();}
		if (properties.mean_houndsfield_value() > maxH) {maxH = properties.mean_houndsfield_value();}
		
		int vox = properties.voxel_count() / (properties.z_max() + 1 - properties.z_min());
		
		if (vox < minVox) { minVox = vox; }
		if (vox > maxVox) { maxVox = vox; }
		
	}
	
	
	std::cout << "organPrefix" << "MaxVox " << maxVox << std::endl;
	std::cout << "organPrefix" << "MinVox " << minVox << std::endl;
	std::cout << "organPrefix" << "MaxH " << maxH << std::endl;
	std::cout << "organPrefix" << "MinH " << minH << std::endl;
	(*m_map)[organPrefix + "MaxVox"] = (int) (maxVox * 1.1);
	std::cout << (*m_map)[organPrefix + "MaxVox"] << std::endl;
	(*m_map)[organPrefix + "MinVox"] = (int) (minVox * 0.9);
	std::cout << (*m_map)[organPrefix + "MinVox"] << std::endl;
	(*m_map)[organPrefix + "MaxH"] = (int) (maxH * 1.1);
	std::cout << (*m_map)[organPrefix + "MaxH"] << std::endl;
	(*m_map)[organPrefix + "MinH"] = (int) (minH * 0.9);
	std::cout << (*m_map)[organPrefix + "MinH"] << std::endl;
	
}

/**
 * Should find minimum and maximum Hounsfield values, and tolerance values for region-grown organs, but currently broken. 
 * 
 * Does not correctly identify tolerance values, so this feature has been removed as no time to fix.
 * 
 */
void SettingsCapturer::get_grow_settings(AbdominalFeature::Enum feature, string organPrefix, int layer, SettingsCapturer::Seed seed) {
	
	PartitionForestSelection_CPtr selection = m_mfs->selection(feature);
	
	PFNodeID best;
	int bestPos;
	
	bool vertical;
	
	
	if (!(*m_map)[organPrefix + "MinH"]) {
		std::cout << "Writing new min h" << std::endl;
		(*m_map)[organPrefix + "MinH"] = 1000;
	}
	
	if (!(*m_map)[organPrefix + "MaxH"]) {
		std::cout << "Writing new max h" << std::endl;
		(*m_map)[organPrefix + "MaxH"] = -1000;
	}
	
	/*if (!(*m_map)[organPrefix + "SeedTolerance"]) {
		std::cout << "Writing new SeedTolerance" << std::endl;
		(*m_map)[organPrefix + "SeedTolerance"] = -1000;
	}
	
	if (!(*m_map)[organPrefix + "AdjTolerance"]) {
		std::cout << "Writing new AdjTolerance" << std::endl;
		(*m_map)[organPrefix + "AdjTolerance"] = -1000;
	}*/
	
	switch (seed) {
		case TOP:
			bestPos = 1000;
			break;
		case BOTTOM:
			bestPos = -1;
			break;
		case LEFT:
			bestPos = 1000;
			break;
		case RIGHT:
			bestPos = -1;
			break;
	}
	
	//Identify the seed
	for (PartitionForestSelectionT::ViewNodeConstIterator it = selection->view_at_layer_cbegin(layer); it != selection->view_at_layer_cend(layer); ++it) {
		
		switch (seed) {
			case TOP:
				if (m_volumeIPF->branch_properties(*it).y_min() < bestPos) {best = *it; bestPos = m_volumeIPF->branch_properties(*it).y_min(); };
				break;
			case BOTTOM:
				if (m_volumeIPF->branch_properties(*it).y_max() > bestPos) {best = *it; bestPos = m_volumeIPF->branch_properties(*it).y_max(); };
				break;
			case LEFT:
				if (m_volumeIPF->branch_properties(*it).x_min() < bestPos) {best = *it; bestPos = m_volumeIPF->branch_properties(*it).x_min(); };
				break;
			case RIGHT:
				if (m_volumeIPF->branch_properties(*it).x_max() > bestPos) {best = *it; bestPos = m_volumeIPF->branch_properties(*it).x_max(); };
				break;
		}
	}
	
	//best is now the seed
	int minH = 10000;
	int maxH = -10000;
	/*int adjTolerance = 0;
	int seedTolerance = 0;*/
	
	BranchProperties seedProperties = m_volumeIPF->branch_properties(best);
	
	for (PartitionForestSelectionT::ViewNodeConstIterator it = selection->view_at_layer_cbegin(layer); it != selection->view_at_layer_cend(layer); ++it) {
		BranchProperties properties = m_volumeIPF->branch_properties(*it);
		
		if (properties.mean_houndsfield_value() < minH) {minH = properties.mean_houndsfield_value();}
		if (properties.mean_houndsfield_value() > maxH) {maxH = properties.mean_houndsfield_value();}
		
		/*if (fabs(seedProperties.mean_houndsfield_value() - properties.mean_houndsfield_value()) > seedTolerance) {
			seedTolerance = fabs(seedProperties.mean_houndsfield_value() - properties.mean_houndsfield_value());
			std::cout << "SeedTolerance = " << seedTolerance << std::endl;
			std::cout << "seed: " << best << ", node: " << *it << endl;
			std::cout << "seed: " << seedProperties.mean_houndsfield_value() << ", node: " << properties.mean_houndsfield_value() << endl;
		}
		
		std::vector<int> adjNodes = m_volumeIPF->adjacent_nodes(*it);
		for(std::vector<int>::const_iterator adjit=adjNodes.begin(), iend=adjNodes.end(); adjit!=iend; ++adjit)
		{
			
			PFNodeID adj(it->layer(), *adjit);
			if (selection->contains(adj)) {
				if (fabs(m_volumeIPF->branch_properties(adj).mean_houndsfield_value() - properties.mean_houndsfield_value()) > adjTolerance) {
					adjTolerance = fabs(m_volumeIPF->branch_properties(adj).mean_houndsfield_value() - properties.mean_houndsfield_value());
					std::cout << "AdjTolerance = " << adjTolerance << std::endl;
					std::cout << "adj: " << adj << ", node: " << *it << endl;
					std::cout << "adj: " << m_volumeIPF->branch_properties(adj).mean_houndsfield_value() << ", node: " << properties.mean_houndsfield_value() << endl;
				}
			}
			
		}*/
		
	}
	
	std::cout << (int) ( minH * 0.9) << ", " << minH * 0.9 << std::endl;
	
	
	if (minH < (*m_map)[organPrefix + "MinH"]) {
		std::cout << "Overwriting min h" << std::endl;
		(*m_map)[organPrefix + "MinH"] = (int) ( minH * 0.9);
	}
	
	
	
	if ((*m_map)[organPrefix + "MaxH"] < maxH ) {
		std::cout << "Overwriting max h" << std::endl;
		(*m_map)[organPrefix + "MaxH"] = (int) (maxH * 1.1);
	}
	
	
	/*if ((*m_map)[organPrefix + "SeedTolerance"] < seedTolerance ) {
		std::cout << "Overwriting SeedTolerance" << std::endl;
		(*m_map)[organPrefix + "SeedTolerance"] = (int) (seedTolerance * 1.1);
	}
	
	if ((*m_map)[organPrefix + "AdjTolerance"] < adjTolerance) {
		std::cout << "Overwriting AdjTolerance" << std::endl;
		(*m_map)[organPrefix + "AdjTolerance"] = (int) (adjTolerance * 1.1);
	}*/
	
	std::cout << "organPrefix" << "MinH " << minH << std::endl;
	std::cout << (*m_map)[organPrefix + "MinH"] << std::endl;
	std::cout << "organPrefix" << "MaxH " << maxH << std::endl;
	std::cout << (*m_map)[organPrefix + "MaxH"] << std::endl;
	/*std::cout << "organPrefix" << "SeedTolerance " << seedTolerance << std::endl;
	std::cout << (*m_map)[organPrefix + "SeedTolerance"] << std::endl;
	std::cout << "organPrefix" << "AdjTolerance " << adjTolerance << std::endl;
	std::cout << (*m_map)[organPrefix + "AdjTolerance"] << std::endl;*/
	
}
	
	
}

