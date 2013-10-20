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

std::map<int,std::vector<int> > make_convergence_chains(const IPF_Ptr& ipf)
{
	std::map<int,std::vector<int> > result;
	for(IPF::LeafNodeConstIterator it=ipf->leaf_nodes_cbegin(), iend=ipf->leaf_nodes_cend(); it!=iend; ++it)
	{
		int leafIndex = it.index();
		PFNodeID cur(0, leafIndex);

		std::vector<int>& chain = result[leafIndex];
		chain.reserve(ipf->highest_layer() + 1);
		while(cur != PFNodeID::invalid())
		{
			chain.push_back(cur.index());
			cur = ipf->parent_of(cur);
		}
	}
	return result;
}

const std::vector<int>& slow_chain(const std::vector<int>& lhs, const std::vector<int>& rhs)
{
	for(size_t i = 0; i < lhs.size() && i < rhs.size(); ++i)
	{
		if(lhs[i] > rhs[i]) return lhs;
		else if(rhs[i] > lhs[i]) return rhs;
	}

	// If we get here, both chains have a common prefix. The "slower" merging of the two
	// is then the shorter one, since the longer one carries on merging.
	return lhs.size() < rhs.size() ? lhs : rhs;
}

std::map<int,std::vector<int> > slow_merge(const std::map<int,std::vector<int> >& lhs, const std::map<int,std::vector<int> >& rhs)
{
	std::map<int,std::vector<int> > result;
	std::map<int,std::vector<int> >::const_iterator it = lhs.begin(), iend = lhs.end(), jt = rhs.begin(), jend = rhs.end();
	while(it != iend && jt != jend)
	{
		result[it->first] = slow_chain(it->second, jt->second);
		++it, ++jt;
	}
	return result;
}

void output_convergence_chains(const std::map<int,std::vector<int> >& chains)
{
	for(std::map<int,std::vector<int> >::const_iterator it=chains.begin(), iend=chains.end(); it!=iend; ++it)
	{
		std::cout << it->first << ": ";
		for(std::vector<int>::const_iterator jt=it->second.begin(), jend=it->second.end(); jt!=jend; ++jt)
		{
			std::cout << *jt;
		}
		std::cout << '\n';
	}
}

IPF_Ptr construct_slow_merged_ipf(const IPF_Ptr& forestX, const IPF_Ptr& forestY)
{
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(*forestX->leaf_layer()));
	for(SimpleImageLeafLayer::LeafNodeIterator it=leafLayer->leaf_nodes_begin(), iend=leafLayer->leaf_nodes_end(); it!=iend; ++it)
	{
		it->set_parent(-1);
	}

	IPF_Ptr result(new IPF(leafLayer));

	std::map<int,std::vector<int> > chainsX = make_convergence_chains(forestX);
	output_convergence_chains(chainsX);

	std::map<int,std::vector<int> > chainsY = make_convergence_chains(forestY);
	output_convergence_chains(chainsY);

	std::map<int,std::vector<int> > chainsSlow = slow_merge(chainsX, chainsY);
	output_convergence_chains(chainsSlow);

	for(int i = 1; i <= forestX->highest_layer() || i <= forestY->highest_layer(); ++i)
	{
		result->clone_layer(result->highest_layer());

		std::map<int,std::set<PFNodeID> > mergeSets;

		size_t layerIndex = result->highest_layer();
		//result->output(std::cout);
		std::cout << "Layer: " << layerIndex << '\n';
		for(IPF::BranchNodeConstIterator jt=result->branch_nodes_cbegin(layerIndex), jend=result->branch_nodes_cend(layerIndex); jt!=jend; ++jt)
		{
			std::cout << jt.index() << ' ' << jt->parent() << '\n';
			const std::vector<int>& chain = chainsSlow[jt.index()];
			if(layerIndex < chain.size())
			{
				mergeSets[chain[layerIndex]].insert(PFNodeID(layerIndex, jt.index()));
			}
		}

		for(std::map<int,std::set<PFNodeID> >::const_iterator jt=mergeSets.begin(), jend=mergeSets.end(); jt!=jend; ++jt)
		{
			std::cout << "Merging ";
			for(std::set<PFNodeID>::const_iterator kt=jt->second.begin(), kend=jt->second.end(); kt!=kend; ++kt)
			{
				std::cout << *kt << '/' << result->parent_of(*kt) << ' ';
			}
			std::cout << std::endl;
			result->merge_sibling_nodes(jt->second);
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

	return 0;
}

