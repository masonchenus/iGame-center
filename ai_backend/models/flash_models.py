
class BaseFlash:
    def __init__(self, tier: str = "flash"):
        self.tier = tier
        self.name = tier

    def generate(self, prompt: str) -> str:
        seed = (self.name + "|" + prompt).encode("utf-8")
        h = hashlib.sha1(seed).hexdigest()[:10]
        return f"[{self.name} simulated | id={h}] {prompt}"

    def generate_tokens(self, text: str) -> list:
        """Generate tokens using the Flash model's token method.

        Flash uses a simplified byte-pair encoding-like approach.
        """
        # Simple BPE-like tokenization: split on whitespace and punctuation
        tokens = re.findall(r"\w+|[.,!?;:]", text)
        return tokens if tokens else text.split()


class FlashModel(BaseFlash):
    def __init__(self):
        super().__init__(tier="flash")

    def generate_tokens(self, text: str) -> list:
        """Flash uses a simplified byte-pair encoding-like approach."""
        # Simple BPE-like tokenization: split on whitespace and punctuation
        tokens = re.findall(r"\w+|[.,!?;:]", text)
        return tokens if tokens else text.split()


class ProFlashModel(BaseFlash):
    def __init__(self):
        super().__init__(tier="pro-flash")

    def generate_tokens(self, text: str) -> list:
        """Pro Flash uses more aggressive tokenization."""
        # Pro Flash: more granular tokens
        tokens = re.findall(r"\w+|[.,!?;:\-]", text)
        return tokens if tokens else text.split()


class UltraModel(BaseFlash):
    def __init__(self):
        super().__init__(tier="ultra")

    def generate_tokens(self, text: str) -> list:
        """Ultra uses character-level + word tokens."""
        # Ultra: hybrid character and word tokenization
        words = text.split()
        tokens = []
        for word in words:
            if len(word) > 4:
                # Long words: split into chunks
                tokens.extend([word[i:i+3] for i in range(0, len(word), 3)])
            else:
                tokens.append(word)
        return tokens if tokens else [text]


class UltraFlashModel(BaseFlash):
    def __init__(self):
        super().__init__(tier="ultra-flash")

    def generate_tokens(self, text: str) -> list:
        """Ultra Flash uses optimized tokenization for speed."""
        # Ultra Flash: minimal tokenization for speed
        return text.split()