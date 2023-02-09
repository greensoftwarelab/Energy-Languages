import os
import os.path
import re
from datetime import datetime

UJOULES = 1
MILLIJOULES = 2
JOULES = 3
WATT_HOURS = 4

def _read_sysfs_file(path):
	with open(path, "r") as f:
		contents = f.read().strip()
		return contents

def _get_domain_info(path):
	name = _read_sysfs_file("%s/name" % path)
	energy_uj = int(_read_sysfs_file("%s/energy_uj" % path))
	max_energy_range_uj = int(_read_sysfs_file("%s/max_energy_range_uj" % path))

	return name, energy_uj, max_energy_range_uj

def _walk_rapl_dir(path):
	regex = re.compile("intel-rapl")

	for dirpath, dirnames, filenames in os.walk(path, topdown=True):		
		for d in dirnames:
			#print d, regex.search(d)
			if not regex.search(d):
				dirnames.remove(d)
		yield dirpath, dirnames, filenames

class RAPLDomain(object):

	@classmethod
	def construct(cls, id, path):
		name, energy_uj, max_energy_range_uj = _get_domain_info(path)

		domain = RAPLDomain()
		domain.name = name
		domain.id = id
		domain.values = {}
		domain.values["energy_uj"] = energy_uj
		domain.max_values = {}
		domain.max_values["energy_uj"] = max_energy_range_uj
		domain.subdomains = {}
		domain.parent = None

		return domain

	def is_subdomain(self):
		splits = self.id.split(":")
		return len(splits) > 2

	def parent_id(self):
		splits = self.id.split(":")
		return ":".join(splits[0:2])

	def print_tree(self):
		print(self)
		for s in self.subdomains:
			self.subdomains[s].print_tree()

	# take the difference of two domain samples, returning a new domain representing that difference
	def __sub__(self, other):
		# make sure that the domains are comparable
		# check the name and id
		assert self.name == other.name and self.id == other.id
		# check to make sure that they have the same values
		for key in self.values:
			assert key in other.values
		# check to make sure that the max values are the same
		for key in self.max_values:
			assert(self.max_values[key] == other.max_values[key])

		# create a new domain to represent the difference
		domain = RAPLDomain()

		# copy the name and id
		domain.name = self.name
		domain.id = self.id

		# calculate the difference of each value
		domain.values = {}
		for v in self.values:
			# take the difference of the values
			diff = self.values[v] - other.values[v]
			# detect if there was a rollover, and handle it
			if diff < 0:
				diff = self.max_values[v] + diff
			# save teh difference in the new domain
			domain.values[v] = diff

		# copy the max values to the new domain
		domain.max_values = {}
		for v in self.max_values:
			domain.max_values[v] = self.max_values[v]

		# set up the subdomain and parent fields
		domain.subdomains = {}
		domain.parent = None

		# return the new domain
		return domain

	def __str__(self):
		values = ""
		for v in self.values:
			values += " %s=%s" % (v, self.values[v])

		values = values.strip()

		return "%s: %s" % (self.name, values)

	def __repr__(self):
		return self.__str__()

class RAPLSample(object):
	
	@classmethod
	def take_sample(cls):
		sample = RAPLSample()
		sample.domains = {}
		sample.domains_by_id = {}
		sample.timestamp = datetime.now()

		for dirpath, dirnames, filenames in _walk_rapl_dir("/sys/class/powercap/intel-rapl"):
			current = dirpath.split("/")[-1]
			splits = current.split(":")

			# base of RAPL tree
			if len(splits) == 1:
				continue

			# package
			elif len(splits) >= 2:
				domain = RAPLDomain.construct(current, dirpath)
				# catalog all domains here
				sample.domains_by_id[domain.id] = domain
				sample._link_tree(domain)

		return sample

	def _link_tree(self, domain):
		if domain.is_subdomain():
			parent = self.domains_by_id[domain.parent_id()]
			parent.subdomains[domain.name] = domain
		else:
			self.domains[domain.name] = domain

	def __sub__(self, other):
		diff = RAPLDifference()
		diff.domains = {}
		diff.domains_by_id = {}
		diff.duration = (self.timestamp - other.timestamp).total_seconds()

		for id in self.domains_by_id:
			assert id in other.domains_by_id

		for id in self.domains_by_id:
			selfDomain = self.domains_by_id[id]
			otherDomain = other.domains_by_id[id]
			diffDomain = selfDomain - otherDomain

			diff.domains_by_id[id] = diffDomain
			diff._link_tree(diffDomain)

		return diff

	def dump(self):
		for domain in self.domains:
			self.domains[domain].print_tree()

	def energy(self, package, domain=None, unit=UJOULES):
		if not domain:
			e = self.domains[package].values["energy_uj"]
		else:
			e = self.domains[package].subdomains[domain].values["energy_uj"]

		if unit == UJOULES:
			return e
		elif unit == MILLIJOULES:
			return e / 1000
		elif unit == JOULES:
			return e / 1000000
		elif unit == WATT_HOURS:
			return e / (1000000*3600)

class RAPLDifference(RAPLSample):

	def average_power(self, package, domain=None):
		return self.energy(package, domain, unit=JOULES) / self.duration

class RAPLMonitor(object):

	@classmethod
	def sample(cls):
		return RAPLSample.take_sample()
