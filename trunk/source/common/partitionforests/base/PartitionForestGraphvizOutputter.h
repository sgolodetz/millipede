/***
 * millipede: PartitionForestGraphvizOutputter.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTGRAPHVIZOUTPUTTER
#define H_MILLIPEDE_PARTITIONFORESTGRAPHVIZOUTPUTTER

#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>

#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>

#include <common/io/util/OSSWrapper.h>
#include "PartitionForestSelection.h"

namespace mp {

/**
@brief	A PartitionForestGraphvizOutputter listens to a PartitionForest and PartitionForestSelection and
		outputs the forest when interesting events happen. The output can then be run through the external
		Graphviz tools to produce a visual representation of the forest.
*/
template <typename LeafLayer, typename BranchLayer, typename Feature>
class PartitionForestGraphvizOutputter
:	public PartitionForest<LeafLayer,BranchLayer>::Listener,
	public PartitionForestSelection<LeafLayer,BranchLayer>::Listener
{
	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef boost::shared_ptr<PartitionForestT> PartitionForest_Ptr;
	typedef PartitionForestSelection<LeafLayer,BranchLayer> PartitionForestSelectionT;
	typedef boost::shared_ptr<PartitionForestSelectionT> PartitionForestSelection_Ptr;
	typedef boost::shared_ptr<const PartitionForestSelectionT> PartitionForestSelection_CPtr;

	typedef typename PartitionForestT::BranchNodeConstIterator BranchNodeConstIterator;
	typedef typename PartitionForestT::EdgeConstIterator EdgeConstIterator;
	typedef typename PartitionForestT::LeafNodeConstIterator LeafNodeConstIterator;
	typedef typename PartitionForestT::NodeConstIterator NodeConstIterator;
	typedef typename PartitionForestSelectionT::Modification Modification;

	//#################### NESTED CLASSES ####################
public:
	/**
	@brief	A LeafPositioner controls where nodes are drawn in the layer graphs.
	*/
	struct LeafPositioner
	{
		virtual ~LeafPositioner() {}
		virtual std::pair<double,double> position_of_node(const PFNodeID& node) const = 0;
	};

	class Grid2DLeafPositioner : public LeafPositioner
	{
	private:
		PartitionForest_Ptr m_forest;
		int m_leafRows, m_leafCols;
	public:
		Grid2DLeafPositioner(const PartitionForest_Ptr& forest, int leafRows, int leafCols)
		:	m_forest(forest), m_leafRows(leafRows), m_leafCols(leafCols)
		{}

		std::pair<double,double> position_of_node(const PFNodeID& node) const
		{
			if(node.layer() > 0)	return position_of_branch(node);
			else					return position_of_leaf(node.index());
		}
	private:
		std::pair<double,double> position_of_branch(const PFNodeID& node) const
		{
			std::pair<double,double> pos(0.0, 0.0);

			std::deque<int> leaves = m_forest->receptive_region_of(node);
			for(std::deque<int>::const_iterator it=leaves.begin(), iend=leaves.end(); it!=iend; ++it)
			{
				std::pair<double,double> leafPos = position_of_leaf(*it);
				pos.first += leafPos.first;
				pos.second += leafPos.second;
			}
			pos.first /= leaves.size();
			pos.second /= leaves.size();

			return pos;
		}

		std::pair<double,double> position_of_leaf(int leafIndex) const
		{
			return std::pair<double,double>(leafIndex % m_leafCols, m_leafRows - leafIndex / m_leafCols);
		}
	};

	/**
	@brief	A StreamController yields the output streams for the Graphviz outputter to write to.
			The streams for the hierarchy and the different layer graphs can be different, and
			they can also change over time. (This last property is useful, because it allows us
			to write the forest to different files as changes are made to it.)
	*/
	struct StreamController
	{
		virtual ~StreamController() {}
		virtual std::ostream& graph_stream(int layer) = 0;
		virtual std::ostream& hierarchy_stream() = 0;
		virtual void output_finished() = 0;
	};

	/**
	@brief	A StdOutputStreamController is a stream controller that yields std::cout as the
			output streams for the Graphviz outputter to write to.
	*/
	struct StdOutputStreamController : StreamController
	{
		std::ostream& graph_stream(int layer)	{ return std::cout; }
		std::ostream& hierarchy_stream()		{ return std::cout; }
		void output_finished()					{ /* No-op */ }
	};

	class FileSequenceStreamController : public StreamController
	{
	private:
		typedef std::map<std::string,boost::shared_ptr<std::ofstream> > StreamMap;
		std::string m_stem;
		StreamMap m_streams;
		unsigned char m_suffix;
	public:
		FileSequenceStreamController(const std::string& stem, unsigned char suffix)
		:	m_stem(stem), m_suffix(suffix)
		{}

		std::ostream& graph_stream(int layerIndex)
		{
			return lookup_stream("graph" + boost::lexical_cast<std::string>(layerIndex));
		}

		std::ostream& hierarchy_stream()
		{
			return lookup_stream("hierarchy");
		}

		void output_finished()
		{
			m_streams.clear();
			++m_suffix;
		}
	private:
		std::ostream& lookup_stream(const std::string& name)
		{
			StreamMap::iterator it = m_streams.find(name);
			if(it == m_streams.end())
			{
				std::ostringstream oss;
				oss << m_stem + '-' << m_suffix << '-' << name << ".gv";
				boost::shared_ptr<std::ofstream> stream(new std::ofstream(oss.str().c_str()));
				it = m_streams.insert(std::make_pair(name, stream)).first;
			}
			return *it->second;
		}
	};

	typedef boost::shared_ptr<LeafPositioner> LeafPositioner_Ptr;
	typedef boost::shared_ptr<StreamController> StreamController_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	bool m_consolidationInterest;
	int m_depthInterest;
	PartitionForest_Ptr m_forest;
	bool m_graphLabels;
	LeafPositioner_Ptr m_leafPositioner;
	PartitionForestSelection_Ptr m_selection;
	StreamController_Ptr m_streamController;

	//#################### CONSTRUCTORS ####################
public:
	PartitionForestGraphvizOutputter(const StreamController_Ptr& streamController,
									 const PartitionForest_Ptr& forest,
									 const boost::optional<PartitionForestSelection_Ptr>& selection = boost::none,
									 const boost::optional<LeafPositioner_Ptr>& leafPositioner = boost::none)
	:	m_consolidationInterest(false),
		m_depthInterest(0),
		m_forest(forest),
		m_graphLabels(false),
		m_leafPositioner(leafPositioner ? *leafPositioner : LeafPositioner_Ptr()),
		m_selection(selection ? *selection : PartitionForestSelection_Ptr()),
		m_streamController(streamController)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void output(const std::string& description, int commandDepth = 0)
	{
		if(commandDepth <= m_depthInterest)
		{
			output_hierarchy(description);
			if(m_leafPositioner)
			{
				for(int i=0; i<=m_forest->highest_layer(); ++i)
				{
					output_layer_graph(i, description);
				}
			}
			m_streamController->output_finished();
		}
	}

	void set_consolidation_interest(bool consolidationInterest)
	{
		m_consolidationInterest = consolidationInterest;
	}

	void set_depth_interest(int depthInterest)
	{
		m_depthInterest = depthInterest;
	}

	void set_graph_labels(bool graphLabels)
	{
		m_graphLabels = graphLabels;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~
	// Common listening methods
	//~~~~~~~~~~~~~~~~~~~~~~~~~

	void command_sequence_execution_ended(const std::string& description, int commandDepth)
	{
		output("Command sequence execution ended: " + description, commandDepth);
	}

	void command_sequence_undo_ended(const std::string& description, int commandDepth)
	{
		output("Command sequence undo ended: " + description, commandDepth);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~
	// Forest listening methods
	//~~~~~~~~~~~~~~~~~~~~~~~~~

	void layer_was_cloned(int index)
	{
		output(OSSWrapper() << "Layer was cloned: " << index, 0);
	}

	void layer_was_deleted(int index)
	{
		output(OSSWrapper() << "Layer was deleted: " << index, 0);
	}

	void layer_was_undeleted(int index)
	{
		output(OSSWrapper() << "Layer was undeleted: " << index, 0);
	}

	void node_was_split(const PFNodeID& node, const std::set<PFNodeID>& results, int commandDepth)
	{
		std::ostringstream oss;
		oss << "Node was split: " << node << " -> { ";
		std::copy(results.begin(), results.end(), std::ostream_iterator<PFNodeID>(oss, " "));
		oss << "}";
		output(oss.str(), commandDepth);
	}

	void nodes_were_merged(const std::set<PFNodeID>& nodes, const PFNodeID& result, int commandDepth)
	{
		std::ostringstream oss;
		oss << "Nodes were merged: { ";
		std::copy(nodes.begin(), nodes.end(), std::ostream_iterator<PFNodeID>(oss, " "));
		oss << "} -> " << result;
		output(oss.str(), commandDepth);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Selection listening methods
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	void modification_redone(const Modification& modification, int commandDepth)
	{
		output("Modification redone", commandDepth);
	}

	void modification_undone(const Modification& modification, int commandDepth)
	{
		output("Modification undone", commandDepth);
	}

	void node_was_consolidated(const PFNodeID& node)
	{
		if(m_consolidationInterest)
		{
			output(OSSWrapper() << "Node was consolidated: " << node);
		}
	}

	void node_was_deconsolidated(const PFNodeID& node)
	{
		if(m_consolidationInterest)
		{
			output(OSSWrapper() << "Node was deconsolidated: " << node);
		}
	}

	void node_was_deselected(const PFNodeID& node, int commandDepth)
	{
		output(OSSWrapper() << "Node was deselected: " << node, commandDepth);
	}

	void node_was_selected(const PFNodeID& node, int commandDepth)
	{
		output(OSSWrapper() << "Node was selected: " << node, commandDepth);
	}

	void selection_changed(int commandDepth)
	{
		output("Selection changed (to reflect a forest change)", commandDepth);
	}

	void selection_was_cleared(int commandDepth)
	{
		output("Selection was cleared", commandDepth);
	}

	void selection_was_replaced(const PartitionForestSelection_CPtr& selection, int commandDepth)
	{
		output("Selection was replaced", commandDepth);
	}

	//#################### PRIVATE METHODS ####################
private:
	void output_layer_graph(int layerIndex, const std::string& description)
	{
		std::ostream& os = m_streamController->graph_stream(layerIndex);

		os << "/* " << description << " - Layer " << layerIndex << " adjacency graph */\n";

		os << "graph\n";
		os << "{\n";

		if(m_graphLabels)
		{
			os << "\tlabel = \"" << description << "\";\n";
		}

		os << "\toverlap = \"prism\";\n";
		os << "\tsep = \"+16\";\n";
		os << "\tnode [shape=circle, style=solid, label=\"\"];\n\n";

		// Output the nodes.
		for(NodeConstIterator it=m_forest->nodes_cbegin(layerIndex), iend=m_forest->nodes_cend(layerIndex); it!=iend; ++it)
		{
			os << "\tn" << it.index() << " [label=\"(" << layerIndex << ',' << it.index() << ")\"";

			std::pair<double,double> pos = m_leafPositioner->position_of_node(PFNodeID(layerIndex, it.index()));
			os << ", pos=\"" << pos.first << ',' << pos.second << "!\"";

			os << "];\n";
		}

		os << '\n';

		// Output the edges between them.
		for(EdgeConstIterator it=m_forest->edges_cbegin(layerIndex), iend=m_forest->edges_cend(layerIndex); it!=iend; ++it)
		{
			os << "\tn" << it->u << " -- n" << it->v << " [label=\"" << it->weight << "\"];\n";
		}

		os << "}\n";
	}

	void output_hierarchy(const std::string& description)
	{
		std::ostream& os = m_streamController->hierarchy_stream();

		os << "/* " << description << " - Hierarchy */\n";

		os << "graph\n";
		os << "{\n";

		if(m_graphLabels)
		{
			os << "\tlabel = \"" << description << "\";\n";
		}

		os << "\tnode [shape=circle, style=filled, label=\"\"];\n\n";

		// Output the nodes.
		for(int i=m_forest->highest_layer(); i>=0; --i)
		{
			for(NodeConstIterator jt=m_forest->nodes_cbegin(i), jend=m_forest->nodes_cend(i); jt!=jend; ++jt)
			{
				os << "\tn" << i << '_' << jt.index() << " [label=\"(" << i << ',' << jt.index() << ")\"";

				if(m_selection && m_selection->in_representation(PFNodeID(i, jt.index())))
				{
					os << ", color=\"red\"";
				}

				// TODO: The multi-feature selection can be visualized by filling the nodes with different colours.
				os << ", fillcolor=\"white\"";

				os << "];\n";
			}
		}

		os << '\n';

		// Output the forest links.
		for(int i=m_forest->highest_layer() - 1; i>=0; --i)
		{
			for(NodeConstIterator jt=m_forest->nodes_cbegin(i), jend=m_forest->nodes_cend(i); jt!=jend; ++jt)
			{
				PFNodeID parent = m_forest->parent_of(PFNodeID(i, jt.index()));
				os << "\tn" << parent.layer() << '_' << parent.index() << " -- " << 'n' << i << '_' << jt.index() << ";\n";
			}
		}

		os << "}\n";
	}
};

}

#endif
