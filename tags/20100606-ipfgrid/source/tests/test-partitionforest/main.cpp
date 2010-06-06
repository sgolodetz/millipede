/***
 * test-partitionforest: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <iostream>

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <common/adts/RootedMST.h>
#include <common/commands/UndoableCommandManager.h>
#include <common/partitionforests/base/PartitionForestMultiFeatureSelection.h>
#include <common/partitionforests/images/SimpleImageBranchLayer.h>
#include <common/partitionforests/images/SimpleImageLeafLayer.h>
using namespace mp;

//#################### TYPEDEFS ####################
typedef PartitionForest<SimpleImageLeafLayer, SimpleImageBranchLayer> IPF;
typedef boost::shared_ptr<IPF> IPF_Ptr;
typedef PartitionForestSelection<SimpleImageLeafLayer, SimpleImageBranchLayer> Selection;
typedef boost::shared_ptr<Selection> Selection_Ptr;

//#################### HELPERS ####################
IPF_Ptr default_ipf(const ICommandManager_Ptr& manager)
{
	// Construct the forest.
	SimplePixelProperties arr[] = {0,1,2,3,4,5,6,7,8};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 3, 3));

	IPF_Ptr ipf(new IPF(leafLayer));

	std::set<PFNodeID> mergees;

	ipf->clone_layer(0);
		mergees.insert(PFNodeID(1,0));	mergees.insert(PFNodeID(1,1));	mergees.insert(PFNodeID(1,3));	mergees.insert(PFNodeID(1,4));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(1,2));	mergees.insert(PFNodeID(1,5));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(1,6));	mergees.insert(PFNodeID(1,7));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(1);
		mergees.insert(PFNodeID(2,2));	mergees.insert(PFNodeID(2,8));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(2);
		mergees.insert(PFNodeID(3,0));	mergees.insert(PFNodeID(3,6));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	ipf->clone_layer(3);
		mergees.insert(PFNodeID(4,0));	mergees.insert(PFNodeID(4,2));
	ipf->merge_sibling_nodes(mergees);	mergees.clear();

	// Make future forest operations undoable.
	ipf->set_command_manager(manager);

	return ipf;
}

//#################### TESTS ####################
enum SimpleFeatureID
{
	KIDNEY,
	LIVER,
};

void feature_selection_test()
{
	ICommandManager_Ptr manager(new UndoableCommandManager);
	IPF_Ptr ipf = default_ipf(manager);

	typedef PartitionForestMultiFeatureSelection<SimpleImageLeafLayer, SimpleImageBranchLayer, SimpleFeatureID> MFS;
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	MFS_Ptr mfs(new MFS(ipf));
	mfs->set_command_manager(manager);

	mfs->identify_feature(PFNodeID(1,6), LIVER);
	mfs->unidentify_feature(PFNodeID(1,6), LIVER);
	manager->undo();
	mfs->clear_feature(LIVER);
	manager->undo();
	mfs->identify_feature(PFNodeID(3,0), KIDNEY);
	std::vector<SimpleFeatureID> features = mfs->features_of(PFNodeID(0,7));
	mfs->clear_all();
	manager->undo();
}

struct ForestListener : IPF::Listener
{
	void layer_was_cloned(int index)		{ std::cout << "Layer cloned: " << index << '\n'; }
	void layer_was_deleted(int index)		{ std::cout << "Layer deleted: " << index << '\n'; }
	void layer_was_undeleted(int index)		{ std::cout << "Layer undeleted: " << index << '\n'; }
	void layer_will_be_deleted(int index)	{ std::cout << "Layer will be deleted: " << index << '\n'; }

	void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results)
	{
		std::cout << "Node split: " << node << " -> { ";
		std::copy(results.begin(), results.end(), std::ostream_iterator<PFNodeID>(std::cout, " "));
		std::cout << "}\n";
	}

	void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result)
	{
		std::cout << "Nodes merged: { ";
		std::copy(nodes.begin(), nodes.end(), std::ostream_iterator<PFNodeID>(std::cout, " "));
		std::cout << "} -> " << result << '\n';
	}

	void nodes_will_be_merged(const std::set<PFNodeID>& nodes)
	{
		std::cout << "Nodes will be merged: { ";
		std::copy(nodes.begin(), nodes.end(), std::ostream_iterator<PFNodeID>(std::cout, " "));
		std::cout << "}\n";
	}
};

void listener_test()
{
	SimplePixelProperties arr[] = {0,1,2,3,4,5,6,7,8};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 3, 3));

	IPF ipf(leafLayer);

	ICommandManager_Ptr manager(new UndoableCommandManager);
	ipf.set_command_manager(manager);

	shared_ptr<ForestListener> listener(new ForestListener);
	ipf.add_listener(listener);

	ipf.clone_layer(0);
	ipf.delete_layer(1);
	manager->undo();
	manager->undo();

	ipf.clone_layer(0);
	std::set<PFNodeID> mergees;
		mergees.insert(PFNodeID(1,2));	mergees.insert(PFNodeID(1,5));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
	ipf.clone_layer(1);
		mergees.insert(PFNodeID(2,0));	mergees.insert(PFNodeID(2,1));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
	manager->undo();
	ipf.delete_layer(2);
	std::vector<std::set<int> > groups;
	std::set<int> group0, group1;
	group0.insert(2);
	group1.insert(5);
	groups.push_back(group0);
	groups.push_back(group1);
	ipf.split_node(PFNodeID(1,2), groups);
	manager->undo();
	ipf.delete_layer(1);
	manager->undo();
	manager->redo();

	ipf.clone_layer(0);
	ipf.clone_layer(1);
	ipf.unzip_node(PFNodeID(0,0), ipf.highest_layer());
	manager->undo();
}

void lowest_branch_layer_test()
{
	// Construct the leaf layer.
	SimplePixelProperties arr[] = {0,1,2,3,4,5,6,7,8};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 3, 3));

	// Create some arbitrary groups.
	std::vector<std::set<int> > groups(3);
	for(int i=0; i<=4; ++i) groups[0].insert(i);
	groups[1].insert(5);
	groups[1].insert(8);
	groups[2].insert(6);
	groups[2].insert(7);

	// Construct the lowest branch layer.
	shared_ptr<SimpleImageBranchLayer> lowestBranchLayer = IPF::make_lowest_branch_layer(leafLayer, groups);

	// Construct the forest itself.
	IPF ipf(leafLayer, lowestBranchLayer);

	// Construct a rooted MST from the lowest branch layer.
	RootedMST<SimpleImageBranchLayer::EdgeWeight> mst(*lowestBranchLayer);
}

void nonsibling_node_merging_test()
{
	// Construct initial forest.
	SimplePixelProperties arr[] = {0,1,2,3,4,5,6,7,8};
	std::vector<SimplePixelProperties> leafProperties(&arr[0], &arr[sizeof(arr)/sizeof(SimplePixelProperties)]);
	shared_ptr<SimpleImageLeafLayer> leafLayer(new SimpleImageLeafLayer(leafProperties, 3, 3));

	IPF ipf(leafLayer);

	ICommandManager_Ptr manager(new UndoableCommandManager);
	ipf.set_command_manager(manager);

	std::set<PFNodeID> mergees;

	ipf.clone_layer(0);

	ipf.clone_layer(1);
		mergees.insert(PFNodeID(2,0));	mergees.insert(PFNodeID(2,1));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(2,2));	mergees.insert(PFNodeID(2,5));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(2,3));	mergees.insert(PFNodeID(2,4));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(2,6));	mergees.insert(PFNodeID(2,7));	mergees.insert(PFNodeID(2,8));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();

	ipf.clone_layer(2);
		mergees.insert(PFNodeID(3,0));	mergees.insert(PFNodeID(3,2));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();
		mergees.insert(PFNodeID(3,3));	mergees.insert(PFNodeID(3,6));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();

	ipf.clone_layer(3);
		mergees.insert(PFNodeID(4,0));	mergees.insert(PFNodeID(4,3));
	ipf.merge_sibling_nodes(mergees);	mergees.clear();

	// Perform operations on it.
	mergees.insert(PFNodeID(1,1));
	mergees.insert(PFNodeID(1,2));
	mergees.insert(PFNodeID(1,3));
	mergees.insert(PFNodeID(1,6));
	ipf.merge_nonsibling_nodes(mergees);

	ipf.output(std::cout);
}

void selection_test()
{
	ICommandManager_Ptr manager(new UndoableCommandManager);
	IPF_Ptr ipf = default_ipf(manager);

	// Test forest selection.
	Selection_Ptr selection(new Selection(ipf));
	selection->set_command_manager(manager);
	ipf->add_listener(selection);

	selection->select_node(PFNodeID(4,0));
	manager->undo();
	manager->redo();
	selection->deselect_node(PFNodeID(0,3));
	manager->undo();

	std::vector<std::set<int> > splitGroups(2);
	splitGroups[0].insert(0);
	splitGroups[1].insert(2);
	ipf->split_node(PFNodeID(4,0), splitGroups);
	manager->undo();

	selection->deselect_node(PFNodeID(0,3));
	ipf->delete_layer(3);
	ipf->delete_layer(2);
	manager->undo();
	manager->undo();
}

void switch_parent_test()
{
	ICommandManager_Ptr manager(new UndoableCommandManager);
	IPF_Ptr ipf = default_ipf(manager);

	ipf->parent_switch(PFNodeID(1,6), 2);
	manager->undo();
	manager->redo();
	manager->undo();

	ipf->output(std::cout);
}

void unzip_zip_test()
{
	ICommandManager_Ptr manager(new UndoableCommandManager);
	IPF_Ptr ipf = default_ipf(manager);

	ipf->unzip_node(PFNodeID(0,5), 3);
	ipf->unzip_node(PFNodeID(0,4), 3);

	std::vector<IPF::Chain> chains;
	IPF::Chain chain0;
	chain0.push_back(PFNodeID(3,4));
	chain0.push_back(PFNodeID(2,4));
	chain0.push_back(PFNodeID(1,4));
	chains.push_back(chain0);
	IPF::Chain chain1;
	chain1.push_back(PFNodeID(3,5));
	chain1.push_back(PFNodeID(2,5));
	chain1.push_back(PFNodeID(1,5));
	chains.push_back(chain1);

	ipf->zip_chains(chains);

	ipf->output(std::cout);
}

int main()
{
	//feature_selection_test();
	//listener_test();
	lowest_branch_layer_test();
	//nonsibling_node_merging_test();
	//selection_test();
	//switch_parent_test();
	//unzip_zip_test();
	return 0;
}
