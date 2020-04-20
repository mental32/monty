from dataclasses import dataclass, field


@dataclass
class MiriVM:
    modules: Dict[str, "Module"] = field(default_factory=dict)

    def process(self, mir: Mir):
        pass

    def run_until_complete(self, name: str, *, args: Dict[str, Any]) -> Any:
        pass
