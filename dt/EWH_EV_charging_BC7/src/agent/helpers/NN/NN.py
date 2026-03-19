import torch.nn as nn

class QNetwork(nn.Module):
    """
    Minimal MLP Q-network.
    - input_dim: length of flattened observation vector
    - output_dim: number of actions (2 for {0,1})
    - hidden_sizes: e.g., (64, 64) for a small, scalable net
    """
    def __init__(self, input_dim: int, output_dim: int = 2, hidden_sizes=(64, 64)):
        super().__init__()
        layers = []
        last = input_dim
        for h in hidden_sizes:
            layers += [nn.Linear(last, h), nn.ReLU()]
            last = h
        layers += [nn.Linear(last, output_dim)]
        self.model = nn.Sequential(*layers)

    def forward(self, x):
        return self.model(x)