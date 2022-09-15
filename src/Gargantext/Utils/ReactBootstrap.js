'use strict';

import * as ReactBootstrap from 'react-bootstrap';

if (typeof window !== 'undefined') {
  window.ReactBootstrap = ReactBootstrap;
}

import * as Alert from 'react-bootstrap/Alert';
import * as OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import * as Popover from 'react-bootstrap/Popover';

let Content = Popover.Content;
let Title = Popover.Title;

export { Alert as alertCpt,
         OverlayTrigger as overlayTriggerCpt,
         Popover as popoverCpt,
         Content as popoverContentCpt,
         Title as popoverTitleCpt };
