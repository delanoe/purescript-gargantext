'use strict';

import Tooltip from 'react-tooltip';

console.log('Tooltip', Tooltip);

if (typeof window !== 'undefined') {
    window.ReactTooltip = Tooltip;
}

export { Tooltip as reactTooltipCpt };
