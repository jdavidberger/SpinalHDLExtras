from collections import defaultdict

class AggregationRegistry:
    def __init__(self):
        self.rules = []  # List of (suffixes, function)

    def register(self, suffixes, func):
        """Register a set of suffixes and a callback.

        func(prefix, matched_fields) gets called where
        matched_fields is {suffix: full_field_name}
        """
        self.rules.append((tuple(suffixes), func))

    def process_event_definitions(self, event_definitions):
        """Scan through all event definitions, searching for matching field groups."""

        for suffixes, func in self.rules:
            group = defaultdict(dict)
            for suffix in suffixes:
                for event in event_definitions:
                    name = event["name"]

                    if name.endswith(suffix):
                        prefix = name[:-len(suffix)]
                        group[prefix][name] = event

            group = {key: value for key, value in group.items() if len(value.keys()) == len(suffixes)}
            func(group)
