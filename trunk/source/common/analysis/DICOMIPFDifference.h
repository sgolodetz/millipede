/***
 * millipede: DICOMIPFDifference.h
 * Jess Pumphrey 2012
 ***/

#ifndef H_MILLIPEDE_DICOMIPFDIFFERENCE
#define H_MILLIPEDE_DICOMIPFDIFFERENCE


#include <iostream>

#include <common/partitionforests/images/VolumeIPF.h>
#include <common/partitionforests/images/VolumeIPFMultiFeatureSelection.h>
#include <common/partitionforests/images/VolumeIPFSelection.h>
#include <deque>
#include <set>

#include <common/math/Vector3.h>

namespace mp {

	
	
/**
 * Selects the volumes where two segmentations differ
 * 
 * This is not commutative, so there are 4 methods:
 * A - B
 * B - A
 * (A - B) n (B - A)
 * (A - B) u (B - A)
 * 
 * 
 */
class DICOMIPFDifference
{
	
	typedef VolumeIPF<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFT;
	typedef boost::shared_ptr<VolumeIPFT> VolumeIPF_Ptr;
	typedef boost::shared_ptr<const VolumeIPFT> VolumeIPF_CPtr;
	
//	typedef PartitionForest<DICOMImageBranchLayer,DICOMImageBranchLayer> PartitionForestT;
//	typedef shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef VolumeIPFSelection<DICOMImageLeafLayer,DICOMImageBranchLayer> VolumeIPFSelectionT;
	typedef boost::shared_ptr<VolumeIPFSelectionT> VolumeIPFSelection_Ptr;
	
private:
	VolumeIPF_Ptr m_ipf1, m_ipf2;
	unsigned l1, l2;
	
public:
	
	DICOMIPFDifference(VolumeIPF_Ptr v1, VolumeIPF_Ptr v2, unsigned l1_, unsigned l2_) {
		m_ipf1 = v1;
		m_ipf2 = v2;
		l1 = l1_;
		l2 = l2_;
	}
	
	
	
	VolumeIPFSelection_Ptr select_differenceBA() {
		swap();
		
		VolumeIPFSelection_Ptr ret = select_differenceAB();
		
		swap();
		
		return ret;
	}
	
	/**
	 * Suppose we are considering (layer a of ipf1) - (layer b or ipf2)
	 * 
	 * - we find all nodes n in ipf1, layer x, and find their corresponding node, n2, in layer y of ipf2
	 *     if n contains its own centroid, we chose the node containing this point
	 *     otherwise, we choose the node containing an arbitrary point in n
	 * - for each leaf node descended from n, we look at its ancestor in layer y f ipf2. If this isn't n2,
	 * we add the leaf node to the selection.
	 */
	std::set<int> find_difference() {
		
		std::set<int> leaves;
		
		//for n in layer l1 of ipf 1
		for (/*typename*/ PartitionForest<DICOMImageLeafLayer,DICOMImageBranchLayer>::BranchNodeConstIterator itnode = m_ipf1->branch_nodes_cbegin(l1); itnode != m_ipf1->branch_nodes_cend(l1); ++itnode) {
		
			//find central point of n
			Vector3d centroidPosition = itnode->properties().centroid();
			itk::Index<3> centroid;
			for(int i=0; i<3; ++i) centroid[i] = NumericUtil::round_to_nearest<int>(centroidPosition[i]);
			
			PFNodeID n2;
			
			//std::cout << "finding n as pfnodeid" << std::endl;
			itk::Index<3> pointInN;
			for(int i=0; i<3; ++i) pointInN[i] = NumericUtil::round_to_nearest<int>(itnode->properties().point_inside()[i]);
			PFNodeID n = m_ipf1->node_of(l1, pointInN);
			
			//set n2 to be a node in layer l2 of ipf2 containing a point from n (the centroid, if n contains its own centroid
			if (n == m_ipf1->node_of(l1, centroid)) {	
				 n2 = m_ipf2->node_of(l2, centroid);
			}
			else {
				n2 = m_ipf2->node_of(l2, pointInN);
			}
			
			//for each leaf descendent of n in ipf1...
			std::deque<int> region = m_ipf1->receptive_region_of(n);
			for (std::deque<int>::iterator itleaf = (region).begin(); itleaf != (region).end(); ++itleaf) {
				
				//find the leaf's position
				itk::Index<3> pos;
				Vector3i pos2 = m_ipf1->position_of(*itleaf);
				for(int i=0; i<3; ++i) pos[i] = NumericUtil::round_to_nearest<int>(pos2[i]);
				
				//find the node at that position in layer l2 of ipf2
				PFNodeID ancestor = m_ipf2->node_of(l2, pos);
				
				//if that's not the node we expected, add the leaf to the selection
				if (ancestor != n2) {
						
					std::cout << pointInN << " - " << pos << " " << centroid << std::endl;
				
					std::cout << n << " ~ " << ancestor << " ~ " << n2 << std::endl;
					std::cout << "node found in difference. leaves.size() = " << leaves.size() << std::endl;
					leaves.insert(*itleaf);
				}
				else {
				}
			}		
		}
		
		return leaves;
		
	}
	
	VolumeIPFSelection_Ptr select_union() {
		
		//make a - b and b - a
		std::set<int> AB = find_difference();
		swap();
		std::set<int> BA = find_difference();
		swap();
		
		std::set<int> unionset = std::set<int>();
		
		std::cout << "calculating union" << std::endl;
		
		for (std::set<int>::iterator it = AB.begin(); it != AB.end(); ++it) {
			unionset.insert(*it);
		}
		for (std::set<int>::iterator it = BA.begin(); it != BA.end(); ++it) {
			unionset.insert(*it);
		}
		
		std::cout << "making selection" << std::endl;
		VolumeIPFSelection_Ptr selection(new VolumeIPFSelectionT(m_ipf1, unionset));
		
		std::cout << "returning" << std::endl;
		return selection;
		//model()->selection()->replace_with_selection(newSelection);
		
	}
	
	VolumeIPFSelection_Ptr select_intersection() {
		
		//make a - b and b - a
		std::set<int> AB = find_difference();
		swap();
		std::set<int> BA = find_difference();
		swap();
		
		std::set<int> intersection = std::set<int>();
		
		
		std::cout << "calculating intersection" << std::endl;
		for (std::set<int>::iterator it = AB.begin(); it != AB.end(); ++it) {
			if (BA.find(*it) != BA.end()) {
				intersection.insert(*it);
				std::cout <<"Found element in intersection" << std::endl;
			}
		}
		
		std::cout << "making selection" << std::endl;
		VolumeIPFSelection_Ptr selection(new VolumeIPFSelectionT(m_ipf1, intersection));
		
		std::cout << "returning" << std::endl;
		return selection;
		//model()->selection()->replace_with_selection(newSelection);
		
	}
	
	
	
	
	VolumeIPFSelection_Ptr select_differenceAB() {
		
		//add to selection
		std::cout << "making selection" << std::endl;
		VolumeIPFSelection_Ptr selection(new VolumeIPFSelectionT(m_ipf1, find_difference()));
		
		std::cout << "returning" << std::endl;
		return selection;
		//model()->selection()->replace_with_selection(newSelection);
		
	}
	
private:
	void swap() {
		VolumeIPF_Ptr t = m_ipf1;
		m_ipf1 = m_ipf2;
		m_ipf2 = t;
		
		unsigned t2 = l1;
		l1 = l2;
		l2 = t2;
	}
	
};

}

#endif
