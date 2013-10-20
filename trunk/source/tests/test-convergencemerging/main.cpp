/***
 * test-convergencemerging: main.cpp
 * Copyright Stuart Golodetz, 2013. All rights reserved.
 ***/

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/commands/UndoableCommandManager.h>
#include <common/partitionforests/base/PartitionForestMultiFeatureSelection.h>
#include <common/partitionforests/graphviz/PartitionForestGraphvizOutputter.h>
#include <common/partitionforests/images/SimpleImageBranchLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
using namespace mp;

//#################### TYPEDEFS ####################
typedef PartitionForest<SimpleImageLeafLayer, SimpleImageBranchLayer> IPF;
typedef PartitionForestGraphvizOutputter<SimpleImageLeafLayer,SimpleImageBranchLayer,int> GVO;

typedef boost::shared_ptr<IPF> IPF_Ptr;

//#################### FUNCTIONS ####################
IPF_Ptr make_forestX(const ICommandManager_Ptr& manager)
{
	// Construct the forest.
	SimplePixelProperties arr[] = {0,1,2,3};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 2, 2));

	IPF_Ptr ipf(new IPF(leafLayer));

	std::set<PFNodeID> mergees;

	ipf->clone_layer(0);
		mergees.insert(PFNodeID(1,0));	mergees.insert(PFNodeID(1,2));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(1);
		mergees.insert(PFNodeID(2,0));	mergees.insert(PFNodeID(2,3));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(2);
		mergees.insert(PFNodeID(3,0));	mergees.insert(PFNodeID(3,1));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	// Make future forest operations undoable.
	ipf->set_command_manager(manager);

	return ipf;
}

IPF_Ptr make_forestY(const ICommandManager_Ptr& manager)
{
	// Construct the forest.
	SimplePixelProperties arr[] = {0,1,2,3};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 2, 2));

	IPF_Ptr ipf(new IPF(leafLayer));

	std::set<PFNodeID> mergees;

	ipf->clone_layer(0);
		mergees.insert(PFNodeID(1,1));	mergees.insert(PFNodeID(1,3));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(1);
		mergees.insert(PFNodeID(2,1));	mergees.insert(PFNodeID(2,2));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(2);
		mergees.insert(PFNodeID(3,0));	mergees.insert(PFNodeID(3,1));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	// Make future forest operations undoable.
	ipf->set_command_manager(manager);

	return ipf;
}

typedef std::map<std::pair<int,int>,int> CommonAncestorMap;

void find_common_ancestor(const IPF_Ptr& ipf, int i, int j, CommonAncestorMap& commonAncestorMap)
{
	int curLayer = 0;
	std::pair<int,int> initial = std::make_pair(i, j);
	std::pair<int,int> cur = initial;
	while(cur.first != -1 && cur.second != -1)
	{
		if(cur.first > cur.second)
		{
			std::swap(cur.first, cur.second);
		}
		else if(cur.first == cur.second)
		{
			commonAncestorMap[initial] = curLayer;
			return;
		}

		std::map<std::pair<int,int>,int>::const_iterator it = commonAncestorMap.find(cur);
		if(it != commonAncestorMap.end())
		{
			commonAncestorMap[initial] = it->second;
			return;
		}

		cur.first = ipf->parent_of(PFNodeID(curLayer, cur.first)).index();
		cur.second = ipf->parent_of(PFNodeID(curLayer, cur.second)).index();
		++curLayer;
	}

	commonAncestorMap[initial] = -1;
}

CommonAncestorMap make_common_ancestor_map(const IPF_Ptr& ipf)
{
	CommonAncestorMap result;

	std::vector<int> leafIndices = ipf->leaf_layer()->node_indices();
	for(size_t i=0, size=leafIndices.size(); i<size-1; ++i)
	{
		for(size_t j=i+1; j<size; ++j)
		{
			find_common_ancestor(ipf, (int)i, (int)j, result);
		}
	}

	return result;
}

CommonAncestorMap slow_merge_ancestor_map(const IPF_Ptr& forestX, const IPF_Ptr& forestY)
{
	CommonAncestorMap result;
	CommonAncestorMap camX = make_common_ancestor_map(forestX);
	CommonAncestorMap camY = make_common_ancestor_map(forestY);
	CommonAncestorMap::const_iterator it = camX.begin(), iend = camX.end(), jt = camY.begin(), jend = camY.end();
	while(it != iend && jt != jend)
	{
		if(it->second > jt->second)
		{
			result.insert(*it);
		}
		else
		{
			result.insert(*jt);
		}
		++it, ++jt;
	}
	return result;
}

CommonAncestorMap fast_merge_ancestor_map(const IPF_Ptr& forestX, const IPF_Ptr& forestY)
{
	CommonAncestorMap result;
	CommonAncestorMap camX = make_common_ancestor_map(forestX);
	CommonAncestorMap camY = make_common_ancestor_map(forestY);
	CommonAncestorMap::const_iterator it = camX.begin(), iend = camX.end(), jt = camY.begin(), jend = camY.end();
	while(it != iend && jt != jend)
	{
		if(it->second < jt->second)
		{
			result.insert(*it);
		}
		else
		{
			result.insert(*jt);
		}
		++it, ++jt;
	}
	return result;
}

typedef std::map<int,std::vector<std::pair<int,int> > > MergeMap;

MergeMap make_merge_map(const CommonAncestorMap& cam)
{
	MergeMap result;
	for(CommonAncestorMap::const_iterator it=cam.begin(), iend=cam.end(); it!=iend; ++it)
	{
		result[it->second].push_back(it->first);
	}
	return result;
}

IPF_Ptr construct_slow_merged_ipf(const IPF_Ptr& forestX, const IPF_Ptr& forestY)
{
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(*forestX->leaf_layer()));
	for(SimpleImageLeafLayer::LeafNodeIterator it=leafLayer->leaf_nodes_begin(), iend=leafLayer->leaf_nodes_end(); it!=iend; ++it)
	{
		it->set_parent(-1);
	}

	IPF_Ptr result(new IPF(leafLayer));

	MergeMap mm = make_merge_map(slow_merge_ancestor_map(forestX, forestY));

	for(int i=1; i<=forestX->highest_layer() || i<=forestY->highest_layer(); ++i)
	{
		result->clone_layer(i-1);

		const std::vector<std::pair<int,int> >& merges = mm[i];
		for(size_t j=0; j<merges.size(); ++j)
		{
			PFNodeID lhs(0,merges[j].first), rhs(0,merges[j].second);
			lhs = result->ancestor_of(lhs, i);
			rhs = result->ancestor_of(rhs, i);
			if(result->has_node(lhs) && result->has_node(rhs))
			{
				std::set<PFNodeID> mergees;
				mergees.insert(lhs);
				mergees.insert(rhs);
				result->merge_nonsibling_nodes(mergees);
			}
		}
	}

	return result;
}

IPF_Ptr construct_fast_merged_ipf(const IPF_Ptr& forestX, const IPF_Ptr& forestY)
{
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(*forestX->leaf_layer()));
	for(SimpleImageLeafLayer::LeafNodeIterator it=leafLayer->leaf_nodes_begin(), iend=leafLayer->leaf_nodes_end(); it!=iend; ++it)
	{
		it->set_parent(-1);
	}

	IPF_Ptr result(new IPF(leafLayer));

	MergeMap mm = make_merge_map(fast_merge_ancestor_map(forestX, forestY));

	/*for(MergeMap::const_iterator it=mm.begin(), iend=mm.end(); it!=iend; ++it)
	{
		std::cout << it->first << ": ";
		for(std::vector<std::pair<int,int> >::const_iterator jt=it->second.begin(), jend=it->second.end(); jt!=jend; ++jt)
		{
			std::cout << '(' << jt->first << ',' << jt->second << ") ";
		}
		std::cout << '\n';
	}*/

	for(int i=1; i<=forestX->highest_layer() || i<=forestY->highest_layer(); ++i)
	{
		result->clone_layer(i-1);

		const std::vector<std::pair<int,int> >& merges = mm[i];
		for(size_t j=0; j<merges.size(); ++j)
		{
			PFNodeID lhs(0,merges[j].first), rhs(0,merges[j].second);
			lhs = result->ancestor_of(lhs, i);
			rhs = result->ancestor_of(rhs, i);
			if(result->has_node(lhs) && result->has_node(rhs))
			{
				std::set<PFNodeID> mergees;
				mergees.insert(lhs);
				mergees.insert(rhs);
				result->merge_nonsibling_nodes(mergees);
			}
		}
	}

	return result;
}

int main()
{
	ICommandManager_Ptr manager(new UndoableCommandManager);
	IPF_Ptr forestX = make_forestX(manager);
	IPF_Ptr forestY = make_forestY(manager);

	//boost::shared_ptr<GVO::StreamController> streamController(new GVO::StdOutputStreamController);
	boost::shared_ptr<GVO::StreamController> streamController(new GVO::FileSequenceStreamController("convergencemerging", 'a'));
	boost::shared_ptr<GVO> gvoX(new GVO(streamController, forestX));
	boost::shared_ptr<GVO> gvoY(new GVO(streamController, forestY));
	forestX->add_shared_listener(gvoX);
	forestY->add_shared_listener(gvoY);
	gvoX->output("Forest X");
	gvoY->output("Forest Y");

	IPF_Ptr forestSlow = construct_slow_merged_ipf(forestX, forestY);
	boost::shared_ptr<GVO> gvoSlow(new GVO(streamController, forestSlow));
	forestSlow->add_shared_listener(gvoSlow);
	gvoSlow->output("Forest Slow");

	IPF_Ptr forestFast = construct_fast_merged_ipf(forestX, forestY);
	boost::shared_ptr<GVO> gvoFast(new GVO(streamController, forestFast));
	forestFast->add_shared_listener(gvoFast);
	gvoFast->output("Forest Fast");

	return 0;
}

