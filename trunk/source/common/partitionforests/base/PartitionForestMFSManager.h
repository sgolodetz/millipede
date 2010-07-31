/***
 * millipede: PartitionForestMFSManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTMFSMANAGER
#define H_MILLIPEDE_PARTITIONFORESTMFSMANAGER

#include "PartitionForestMultiFeatureSelection.h"

namespace mp {

template <typename MFS>
class PartitionForestMFSManager
{
	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void active_multi_feature_selection_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void active_multi_feature_selection_changed()
		{
			multicast(boost::bind(&Listener::active_multi_feature_selection_changed, _1));
		}
	};

	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	typedef boost::shared_ptr<const MFS> MFS_CPtr;
	typedef std::map<std::string,MFS_Ptr> MFSMap;

	//#################### PRIVATE VARIABLES ####################
private:
	MFS_Ptr m_activeMultiFeatureSelection;
	ICommandManager_Ptr m_commandManager;
	CompositeListener m_listeners;
	MFSMap m_multiFeatureSelections;

	//#################### CONSTRUCTORS ####################
public:
	PartitionForestMFSManager(const std::string& initialMFSName, const MFS_Ptr& initialMFS)
	:	m_activeMultiFeatureSelection(initialMFS), m_commandManager(new BasicCommandManager)
	{
		m_multiFeatureSelections.insert(std::make_pair(initialMFSName, initialMFS));
	}

	//#################### PUBLIC METHODS ####################
public:
	const MFS_Ptr& active_multi_feature_selection()
	{
		return m_activeMultiFeatureSelection;
	}

	MFS_CPtr active_multi_feature_selection() const
	{
		return m_activeMultiFeatureSelection;
	}

	const MFSMap& multi_feature_selections() const
	{
		return m_multiFeatureSelections;
	}

	void set_active_multi_feature_selection(const std::string& name)
	{
		typename MFSMap::iterator it = m_multiFeatureSelections.find(name);
		if(it != m_multiFeatureSelections.end())
		{
			m_activeMultiFeatureSelection = it->second;
			m_listeners.active_multi_feature_selection_changed();
		}
		else throw Exception("Multi-feature selection " + name + " does not exist");
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
		for(typename MFSMap::iterator it=m_multiFeatureSelections.begin(), iend=m_multiFeatureSelections.end(); it!=iend; ++it)
		{
			it->second->set_command_manager(commandManager);
		}
	}
};

}

#endif
