import { visit } from 'unist-util-visit';

export function affiliateLinkPlugin() {
  return (tree) => {
    visit(tree, 'element', (node) => {
      if (node.tagName === 'a' && node.properties?.href) {
        const href = node.properties.href;

        if (href.includes('amazon.com') || href.includes('amzn.to')) {
          const hasPaidText = node.children?.some(child =>
            typeof child.value === 'string' && child.value.includes('(paid link)')
          );

          if (!hasPaidText) {
            node.children.push({
              type: 'text',
              value: ' (paid link)'
            });
          }

          // Optional but recommended
          node.properties.rel = (node.properties.rel || '')
            ? `${node.properties.rel} sponsored`
            : 'sponsored';
        }
      }
    });
  };
}
